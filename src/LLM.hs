{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module handles the interaction with the Large Language Model (LLM).
-- It includes functions for converting between the project's domain types and the OpenAI library types,
-- making API requests, and implementing the core logic for tool execution.
module LLM
  ( llmSingleTurnAgent,
    llmSingleTurnAgentWithToolExecution,
    LLMAgentContext (..),
  )
where

import Agent
import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Foldable (find, traverse_)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import OpenAI.V1
import qualified OpenAI.V1.Chat.Completions as C
import qualified OpenAI.V1.Tool as T
import OpenAI.V1.ToolCall as TC
import Types

-- | Converts a domain `FunctionCall` to an OpenAI `ToolCall`.
domainToOpenAIToolCall :: FunctionCall -> ToolCall
domainToOpenAIToolCall (FunctionCall {id = callId, function = FunctionCallParams {name = funcName, arguments = funcArgs}}) =
  ToolCall_Function
    {
      id = Text.pack callId,
      function =
        TC.Function
          {
            name = Text.pack funcName,
            arguments = TL.toStrict . encodeToLazyText $ funcArgs
          }
    }

-- | Converts a domain `Chat` message to an OpenAI `Message`.
domainToOpenAIMessage :: Chat -> C.Message (V.Vector C.Content)
domainToOpenAIMessage (SystemMessage m) =
  let content = [C.Text {text = m}]
      name = Nothing
   in C.System {content, name}
domainToOpenAIMessage (UserMessage m) =
  let content = [C.Text {text = m}]
      name = Nothing
   in C.User {content, name}
domainToOpenAIMessage (AssistantMessage {text = maybeText, functionCalls = fs}) =
  let assistant_content = case maybeText of
        Nothing -> Nothing
        Just t -> Just [C.Text {text = t}]
      tool_calls = getToolCalls fs
      name = Nothing
      refusal = Nothing
      assistant_audio = Nothing
   in C.Assistant {assistant_content, name, refusal, assistant_audio, tool_calls}
  where
    getToolCalls [] = Nothing
    getToolCalls as = Just . V.fromList . fmap domainToOpenAIToolCall $ as
domainToOpenAIMessage (ToolMessage {id = tcId, response = tcResponse}) =
  let content = [C.Text {text = maybe Text.empty (TL.toStrict . encodeToLazyText) tcResponse}]
      tool_call_id = Text.pack tcId
   in C.Tool {tool_call_id, content}

-- | Converts a domain `AnyTool` to an OpenAI `Tool`.
domainToOpenAITool :: AnyTool -> T.Tool
domainToOpenAITool (AnyTool (Tool {name, description, parameters})) =
  T.Tool_Function
    {
      T.function =
        T.Function
          {
            T.name = Text.pack name,
            T.description = Just . Text.pack $ description,
            T.parameters = Just parameters,
            T.strict = Just False
          }
    }

-- | Converts an OpenAI `ToolCall` to a domain `FunctionCall`.
openAIToDomainToolCall :: ToolCall -> FunctionCall
openAIToDomainToolCall (ToolCall_Function {id = tcId, function = tcFunction}) =
  FunctionCall
    {
      id = Text.unpack tcId,
      function =
        FunctionCallParams
          {
            name = Text.unpack (TC.name tcFunction),
            arguments = case eitherDecode . TLE.encodeUtf8 . TL.fromStrict . TC.arguments $ tcFunction of
              Left err -> object ["error" .= err]
              Right val -> val
          }
    }

-- | Converts an OpenAI `Message` to a domain `Chat`.
-- Note: This function is partial and will return an error for `Tool` messages from OpenAI,
-- as they are not expected in the assistant's response.
openAIMessageToDomainChat :: C.Message Text.Text -> Either String Chat
openAIMessageToDomainChat (C.System {content}) = Right . SystemMessage $ content
openAIMessageToDomainChat (C.User {content}) = Right . UserMessage $ content
openAIMessageToDomainChat (C.Assistant {assistant_content = text, tool_calls}) =
  let functionCalls = case tool_calls of
        Nothing -> []
        Just fs -> V.toList $ fmap openAIToDomainToolCall fs
   in Right $ AssistantMessage {text, functionCalls}
openAIMessageToDomainChat (C.Tool {}) = Left "OpenAI tool response to domain chat not implemented"

-- | Creates a `CreateChatCompletion` request payload from the agent's context and chat history.
createChatCompletionRequest :: LLMAgentContext -> [Chat] -> C.CreateChatCompletion
createChatCompletionRequest ctx cs =
  C._CreateChatCompletion
    {
      C.messages = V.fromList $ system : users,
      C.tools = case modelTools of
        [] -> Nothing
        ts -> Just . V.fromList $ ts,
      C.model = openAIModel ctx
    }
  where
    system = domainToOpenAIMessage . SystemMessage . systemPrompt $ ctx
    users = fmap domainToOpenAIMessage cs
    modelTools = fmap domainToOpenAITool (tools ctx)

-- | Makes a single chat completion request to the OpenAI API.
-- It takes the agent context and current chat history, and returns either an error or the assistant's response.
makeLLMRequest :: LLMAgentContext -> [Chat] -> IO (Either String Chat)
makeLLMRequest _ [] = return $ Left "No Message"
makeLLMRequest ctx cs = runExceptT $ do
  let Methods {createChatCompletion} = makeMethods (clientEnv ctx) (Text.pack . apiKey $ ctx) Nothing Nothing

  apiResp <- liftIO . try . createChatCompletion $ createChatCompletionRequest ctx cs

  choices <- case apiResp of
    Left (e :: SomeException) -> throwE (show e)
    Right result -> return $ C.choices result

  if V.null choices
    then throwE "No choices returned from LLM"
    else
      let firstChoice = V.head choices
          message = C.message firstChoice
       in liftEither $ openAIMessageToDomainChat message

-- | An agent that performs a single turn of conversation with the LLM, without any tool execution.
-- It sends the current chat history and returns the LLM's response.
llmSingleTurnAgent :: Agent LLMAgentContext Chat
llmSingleTurnAgent = do
  s <- get
  result <- liftIO $ makeLLMRequest (context s) (chats s)
  case result of
    Left err -> throwError (err, s)
    Right c -> return c

-- | An agent that executes a single function call requested by the LLM.
-- It finds the corresponding tool, parses the arguments, executes the tool's function,
-- and returns the result as a `ToolMessage`.
executeFunctionAgent :: FunctionCall -> Agent LLMAgentContext Chat
executeFunctionAgent FunctionCall {id = fcId, function = f} = do
  s <- get

  let FunctionCallParams {name = toolName, arguments = toolArgs} = f
      toolList = tools (context s)

  case find (\(AnyTool (Tool {name})) -> name == toolName) toolList of
    Nothing -> throwError ("Tool not found " ++ toolName, s)
    Just (AnyTool Tool {execute}) -> do
      case fromJSON toolArgs of
        Error err -> throwError ("Failed to parse arguments for tool " ++ toolName ++ ": " ++ err, s)
        Success args -> do
          result <- execute args
          return $ ToolMessage {id = fcId, response = Just (toJSON result)}

-- | An agent that performs a single conversational turn with the LLM, including tool execution.
-- 1. It first calls the LLM.
-- 2. If the LLM returns a standard message, it returns the message.
-- 3. If the LLM requests function calls, it appends the request to the chat history,
--    executes each function, and then calls the LLM *again* with the tool results.
-- 4. This second LLM call provides the final response for the turn.
llmSingleTurnAgentWithToolExecution :: Agent LLMAgentContext Chat
llmSingleTurnAgentWithToolExecution = do
  resp <- llmSingleTurnAgent
  case resp of
    assistantMessage@AssistantMessage {functionCalls = fs@(_ : _)} -> do
      -- we need to persist the tool call into the history before calling them
      modify $ \s -> s {chats = chats s ++ [assistantMessage]}
      traverse_ (appendOutput . executeFunctionAgent) fs
      llmSingleTurnAgent
    _ -> return resp
