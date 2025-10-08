{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM
  ( llmSingleTurnAgent,
    Tool (..),
    LLMAgentContext (..),
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import OpenAI.V1
import qualified OpenAI.V1.Chat.Completions as C
import OpenAI.V1.Models (Model)
import qualified OpenAI.V1.Tool as T
import OpenAI.V1.ToolCall as TC
import Servant.Client (ClientEnv)
import Types

-- | A tool that the LLM can use.
data Tool = FunctionTool
  { name :: String,
    description :: String,
    parameters :: Value
  }

-- | The context required for an LLM agent.
data LLMAgentContext = LLMAgentContext
  { -- | The OpenAI model to use.
    openAIModel :: Model,
    -- | The OpenAI API key.
    apiKey :: String,
    -- | The Servant client environment.
    clientEnv :: ClientEnv,
    -- | A system prompt that guides the LLM's behavior.
    systemPrompt :: Text.Text,
    -- | A list of tools that the LLM can use.
    tools :: [Tool]
  }

-- | Converts a `FunctionCall` to an OpenAI `ToolCall`.
domainToOpenAIToolCall :: FunctionCall -> ToolCall
domainToOpenAIToolCall (FunctionCall {id = callId, function = FunctionCallParams {name = funcName, arguments = funcArgs}}) =
  ToolCall_Function
    { id = Text.pack callId,
      function =
        TC.Function
          { name = Text.pack funcName,
            arguments = TL.toStrict . encodeToLazyText $ funcArgs
          }
    }

-- | Converts a `Chat` to an OpenAI `Message`.
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
      tool_calls = Just $ V.fromList . fmap domainToOpenAIToolCall $ fs
      name = Nothing
      refusal = Nothing
      assistant_audio = Nothing
   in C.Assistant {assistant_content, name, refusal, assistant_audio, tool_calls}

-- | Converts a `Tool` to an OpenAI `Tool`.
domainToOpenAITool :: Tool -> T.Tool
domainToOpenAITool FunctionTool {name, description, parameters} =
  T.Tool_Function
    { T.function =
        T.Function
          { T.name = Text.pack name,
            T.description = Just . Text.pack $ description,
            T.parameters = Just parameters,
            T.strict = Just False
          }
    }

-- | Converts an OpenAI `ToolCall` to a `FunctionCall`.
openAIToDomainToolCall :: ToolCall -> FunctionCall
openAIToDomainToolCall (ToolCall_Function {id = tcId, function = tcFunction}) =
  FunctionCall
    { id = Text.unpack tcId,
      function =
        FunctionCallParams
          { name = Text.unpack (TC.name tcFunction),
            arguments = case eitherDecode . TLE.encodeUtf8 . TL.fromStrict . TC.arguments $ tcFunction of
              Left err -> object ["error" .= err]
              Right val -> val
          }
    }

-- | Converts an OpenAI `Message` to a `Chat`.
openAIMessageToDomainChat :: C.Message Text.Text -> Either String Chat
openAIMessageToDomainChat (C.System {content}) = Right . SystemMessage $ content
openAIMessageToDomainChat (C.User {content}) = Right . UserMessage $ content
openAIMessageToDomainChat (C.Assistant {assistant_content = text, tool_calls}) =
  let functionCalls = case tool_calls of
        Nothing -> []
        Just fs -> V.toList $ fmap openAIToDomainToolCall fs
   in Right $ AssistantMessage {text, functionCalls}
openAIMessageToDomainChat (C.Tool {}) = Left "Tool messages are not supported yet"

-- | Creates a chat completion request for the OpenAI API.
createChatCompletionRequest :: LLMAgentContext -> [Chat] -> C.CreateChatCompletion
createChatCompletionRequest ctx cs =
  C._CreateChatCompletion
    { C.messages = V.fromList $ system : users,
      C.tools = case modelTools of
        [] -> Nothing
        ts -> Just . V.fromList $ ts,
      C.model = openAIModel ctx
    }
  where
    system = domainToOpenAIMessage . SystemMessage . systemPrompt $ ctx
    users = fmap domainToOpenAIMessage cs
    modelTools = fmap domainToOpenAITool (tools ctx)

-- | Makes a call to the LLM.
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

-- | An agent that performs a single turn of conversation with the LLM.
llmSingleTurnAgent :: Agent LLMAgentContext Chat
llmSingleTurnAgent = do
  Session {chats, context} <- get
  result <- liftIO $ makeLLMRequest context chats
  liftEither result