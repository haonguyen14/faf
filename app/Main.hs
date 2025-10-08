{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import LLM
import OpenAI.V1
import qualified System.Environment as Environment
import Types
import Agent

emptySession :: Session ()
emptySession = Session {chats = [], context = ()}

temperatureTool :: Tool
temperatureTool =
  FunctionTool
    { name = "temperature_tool",
      description = "Tool to get temperature at a given zipcode",
      parameters =
        object
          [ "type" .= ("object" :: String),
            "properties"
              .= object
                [ "zipcode"
                    .= object
                      [ "type" .= ("string" :: String),
                        "description" .= ("The zipcode to get the temperature for." :: String)
                      ]
                ],
            "required" .= (["zipcode"] :: [String])
          ]
    }

main :: IO ()
main = do
  putStrLn "--- Initializing Environment ---"
  key <- Environment.getEnv "OPENAI_KEY"
  client <- getClientEnv "https://api.openai.com"

  let llmSession =
        Session
          { chats = [UserMessage "Hello, I live in Seattle. What is the temperature right now?"],
            context =
              LLMAgentContext
                { apiKey = key,
                  clientEnv = client,
                  openAIModel = "gpt-4o-mini",
                  systemPrompt = "You are a comedian. Always response with a funny tone. Use tools to answer question",
                  tools = [temperatureTool]
                }
          }

  putStrLn "--- Running LLM Agent ---"
  result1 <- runAgent llmSingleTurnAgent llmSession
  case result1 of
    Left err -> putStrLn $ "Error: " ++ err
    Right (chat, _) -> print chat

  putStrLn "\n--- Running Repeat Agent with non-empty chat ---"
  result2 <- runAgent repeatAgent llmSession
  case result2 of
    Left err -> putStrLn $ "Error: " ++ err
    Right (chat, _) -> print chat

  putStrLn "\n--- Running Repeat Agent with empty chat ---"
  result3 <- runAgent repeatAgent emptySession
  case result3 of
    Left err -> putStrLn $ "Error: " ++ err
    Right (chat, _) -> print chat

  putStrLn "\n--- Chaining multiple LLM Agents (Monadic) ---"
  let chainedAgent = sequentialAgent [llmSingleTurnAgent, repeatAgent]
  chainedResult <- runAgent chainedAgent llmSession
  case chainedResult of
    Left err -> putStrLn $ "Error: " ++ err
    Right (_, s) -> do
      print . show . chats $ s

  putStrLn "\n--- Looping Repeat Agent ---"
  let loopingAgent = loopAgent (quitAfterNTerminator 2) repeatAgent
  loopResult <- runAgent loopingAgent llmSession
  case loopResult of
    Left err -> putStrLn $ "Error: " ++ err
    Right (_, s) -> do
      print . show . chats $ s