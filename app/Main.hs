{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import qualified Data.Text as Text
import LLM
import OpenAI.V1
import qualified System.Environment as Environment
import Text.Printf (printf)
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
getTemperature :: TemperatureInput -> Agent LLMAgentContext TemperatureOutput
getTemperature (TemperatureInput {zipcode = z}) = do
  -- In a real application, you would call an external API here.
  liftIO $ printf "--- Tool Execution: Getting temperature for zipcode %s ---\n" z
  return $ TemperatureOutput {temperature = 72, unit = "F"}

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

main :: IO ()
main = do
  -- First, we set up the environment, including the OpenAI API key and the client.
  putStrLn "--- Initializing Environment ---"
  key <- Environment.getEnv "OPENAI_KEY"
  client <- getClientEnv "https://api.openai.com"

  -- We create the initial session for the agent.
  let llmSession =
        Session
          { -- The session starts with a user message.
            chats = [UserMessage "Hello, I live in Seattle, 98101. What is the temperature right now?"],
            -- The context provides the agent with everything it needs to run,
            -- including the API key, the model to use, and the list of available tools.
            context =
              LLMAgentContext
                { apiKey = key,
                  clientEnv = client,
                  openAIModel = "gpt-4o-mini",
                  systemPrompt = "You are a helpful assistant. Use tools to answer questions.",
                  tools = [temperatureTool]
                }
          }

  -- We run the agent with the initial session.
  -- `llmSingleTurnAgentWithToolExecution` will first call the LLM,
  -- and if the LLM requests a tool, it will execute the tool.
  putStrLn "\n--- Running LLM Agent with Tool Execution ---"
  let agent = llmSingleTurnAgentWithToolExecution
  result <- runAgent agent llmSession

  -- Finally, we print the result of the agent's execution.
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right (chat, s) -> do
      putStrLn "\n--- Agent's Final Response ---"
      print chat
      putStrLn "\n--- Full Chat History ---"
      print (chats s)
