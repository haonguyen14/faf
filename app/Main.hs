{-# LANGUAGE OverloadedStrings #-}

module Main where

import Agent
import qualified Data.Text.IO as TIO
import LLM
import OpenAI.V1
import qualified System.Environment as Environment
import Types

emptySession :: Session ()
emptySession = Session {chats = [], context = ()}

main :: IO ()
main = do
  putStrLn "--- Initializing Environment ---"
  key <- Environment.getEnv "OPENAI_KEY"
  client <- getClientEnv "https://api.openai.com"

  let llmSession =
        Session
          { chats = ["Hello"],
            context =
              LLMAgentContext
                { apiKey = key,
                  clientEnv = client,
                  openAIModel = "gpt-4o-mini",
                  systemPrompt = "You are a comedian. Always response with a funny tone"
                }
          }

  putStrLn "--- Running LLM Agent ---"
  result1 <- runAgent llmSingleTurnAgent llmSession
  case result1 of
    Left err -> putStrLn $ "Error: " ++ err
    Right (chat, _) -> TIO.putStrLn chat

  -- putStrLn "\n--- Running Repeat Agent with non-empty chat ---"
  -- result2 <- runAgent repeatAgent llmSession
  -- case result2 of
  --   Left err -> putStrLn $ "Error: " ++ err
  --   Right (chat, _) -> TIO.putStrLn chat

  -- putStrLn "\n--- Running Repeat Agent with empty chat ---"
  -- result3 <- runAgent repeatAgent emptySession
  -- case result3 of
  --   Left err -> putStrLn $ "Error: " ++ err
  --   Right (chat, _) -> TIO.putStrLn chat

  -- putStrLn "\n--- Chaining multiple LLM Agents (Monadic) ---"
  -- let chainedAgent = sequentialAgent [llmSingleTurnAgent, llmSingleTurnAgent]
  -- chainedResult <- runAgent chainedAgent llmSession
  -- case chainedResult of
  --   Left err -> putStrLn $ "Error: " ++ err
  --   Right (_, s) -> do
  --     print . show . chats $ s

  -- putStrLn "\n--- Looping Repeat Agent ---"
  -- let loopingAgent = loopAgent (quitAfterNTerminator 2) repeatAgent
  -- loopResult <- runAgent loopingAgent llmSession
  -- case loopResult of
  --   Left err -> putStrLn $ "Error: " ++ err
  --   Right (_, s) -> do
  --     print . show . chats $ s