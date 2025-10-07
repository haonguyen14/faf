{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM (llmSingleTurnAgent, LLMAgentContext (..)) where

import Control.Exception (SomeException, try)
import Control.Monad.Trans.Except
import qualified Data.Text as Text
import qualified Data.Vector as V
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import OpenAI.V1.Models (Model)
import Servant.Client (ClientEnv)
import Types

data LLMAgentContext = LLMAgentContext
  { openAIModel :: Model,
    apiKey :: String,
    clientEnv :: ClientEnv,
    systemPrompt :: Text.Text
  }

chatsToMessages :: [Chat] -> [Message (V.Vector Content)]
chatsToMessages = fmap toUserMessage
  where
    -- Text is the Content type in OpenAI package
    toUserMessage chat = User {content = [Text {text = chat}], name = Nothing}

systemMessage :: Text.Text -> Message (V.Vector Content)
systemMessage prompt = System {content = [Text {text = prompt}], name = Nothing}

llmCall :: LLMAgentContext -> [Chat] -> IO (Either String Chat)
llmCall _ [] = return $ Left "No Message"
llmCall ctx cs = runExceptT $ do
  let Methods {createChatCompletion} = makeMethods (clientEnv ctx) (Text.pack . apiKey $ ctx) Nothing Nothing

  apiResp <-
    liftIO . try . createChatCompletion $
      _CreateChatCompletion
        { messages = V.fromList ((systemMessage . systemPrompt $ ctx) : chatsToMessages cs),
          model = openAIModel ctx
        }

  case apiResp of
    Left (e :: SomeException) -> throwE (show e)
    Right result -> return $ foldr (Text.append . messageToContent . message) "" (choices result)

llmSingleTurnAgent :: Agent LLMAgentContext Chat
llmSingleTurnAgent = do
  Session {chats, context} <- get
  result <- liftIO $ llmCall context chats
  case result of
    Left err -> throwError err
    Right a -> return a