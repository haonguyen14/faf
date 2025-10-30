{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides an example of how to use the LLM agent framework.
-- It demonstrates how to define a tool, initialize an agent, and run it in a conversational loop.
module Main where

import Agent (loopAgent, Streamer (runStreamer))
import Data.Aeson
import qualified Data.Text as Text
import LLM
import OpenAI.V1
import qualified System.Environment as Environment
import Types

-- This file provides an example of how to use the LLM agent framework.

-- == 1. Defining a Tool ==

-- To create a tool, we first define the types for its input and output.

-- Input type for the temperature tool.
-- The FromJSON instance allows this type to be created from the JSON arguments provided by the LLM.
newtype TemperatureInput = TemperatureInput {zipcode :: String}

instance FromJSON TemperatureInput where
  parseJSON = withObject "TemperatureInput" $ \v -> TemperatureInput <$> v .: "zipcode"

-- Output type for the temperature tool.
-- The ToJSON instance allows the output of the tool to be converted into JSON
-- so it can be sent back to the LLM.
data TemperatureOutput = TemperatureOutput
  { temperature :: Double,
    unit :: Text.Text
  }

instance ToJSON TemperatureOutput where
  toJSON (TemperatureOutput {temperature = t, unit = u}) = object ["temperature" .= t, "unit" .= u]

-- This is the function that the tool will execute.
-- It takes the tool's input type and returns the output type within the Agent monad,
-- allowing it to perform IO and access agent state.
-- This is the function that the tool will execute.
-- It takes the tool's input type and returns the output type within the Agent monad,
-- allowing it to perform IO and access agent state.
getTemperature :: TemperatureInput -> Agent LLMAgentContext TemperatureOutput
getTemperature (TemperatureInput {zipcode = z}) = do
  -- In a real application, you would call an external API here.
  return $ TemperatureOutput {temperature = 72, unit = "F"}

-- Now, we define the tool itself.
-- It includes metadata like name and description, a schema for the expected parameters,
-- and the actual Haskell function to execute.
-- Now, we define the tool itself.
-- It includes metadata like name and description, a schema for the expected parameters,
-- and the actual Haskell function to execute.
temperatureTool :: AnyTool
temperatureTool =
  -- We wrap the tool in `AnyTool` to allow for lists of tools with different
  -- input and output types.
  AnyTool $
    Tool
      { -- The name of the tool
        name = "get_temperature",
        -- A description for the LLM to understand what the tool does.
        description = "Get the current temperature for a given zipcode.",
        -- A JSON schema describing the parameters the tool expects.
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
            ],
        -- The function to execute when the tool is called.
        execute = getTemperature
      }

-- == 2. Running the Agent ==

-- | An agent that reads a line of input from the console and adds it to the chat history as a `UserMessage`.
getUserPrompt :: Agent ctx (Maybe ())

getUserPrompt = do
  p <- liftIO getLine
  modify $ \s -> s {chats = chats s ++ [UserMessage (Text.pack p)]}
  return Nothing

-- | An agent that consumes a stream of `Chat` messages and prints each one to the console.
printStream :: AgentStream' ctx Chat -> Agent ctx ()

printStream Nil = return ()
printStream (Cons a as) = do
  liftIO $ print a
  rest <- as
  printStream rest

-- | The main entry point for the application.
main :: IO ()

main = do
  -- First, we set up the environment, including the OpenAI API key and the client.
  putStrLn "--- Initializing Environment ---"
  key <- Environment.getEnv "OPENAI_KEY"
  client <- getClientEnv "https://api.openai.com"

  let ctx =
        LLMAgentContext
          { apiKey = key,
            clientEnv = client,
            openAIModel = "gpt-4o-mini",
            systemPrompt = "You are a helpful assistant. Use tools to answer questions.",
            tools = [temperatureTool]
          }

  let session = Session {chats = [], context = ctx}

  putStrLn "\n--- Running LLM Agent with loopAgent ---"
  let agentLoop = loopAgent getUserPrompt llmSingleTurnAgentWithToolExecution
  let consumeStream = runStreamer agentLoop >>= printStream
  result <- runAgent consumeStream session

  case result of
    Left (err, s) -> do
      putStrLn "Error has occured"
      putStrLn err
      print s
    Right (_, _) -> putStrLn "Finished!!"
