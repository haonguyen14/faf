# FAF - A Haskell Agent Framework

FAF is a simple Haskell framework for building agents that interact with Large Language Models (LLMs). It provides a monadic interface for creating complex agentic workflows.

## Features

- **Monadic Agent Interface**: Define complex agent behaviors using the `Agent` monad, which manages state and error handling.
- **LLM Integration**: Easily interact with OpenAI's chat completion API.
- **Tool Use**: Define and use tools that the LLM can call.
- **Agent Composition**: Combine simple agents to create more complex ones.

## Getting Started

### Prerequisites

- [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/)
- An OpenAI API key

### Building and Running

1.  Clone the repository:
    ```bash
    git clone https://github.com/your-username/faf.git
    cd faf
    ```

2.  Set your OpenAI API key as an environment variable:
    ```bash
    export OPENAI_KEY="your-api-key"
    ```

3.  Build and run the example:
    ```bash
    stack build
    stack exec faf-exe
    ```

## Architecture

The framework is built around a few key concepts:

-   **`Agent` Monad**: The `Agent` monad is a monad that manages the agent's state, including the conversation history (`chats`) and a context (`ctx`).

-   **`Session`**: The `Session` data type holds the current state of the agent, including the list of `Chat` messages and a user-defined `context`.

-   **`LLM` Module**: This module provides the `llmSingleTurnAgent` agent, which performs a single turn of conversation with the LLM. It also includes functions for converting between the framework's internal data types and the OpenAI API's data types.

-   **`Agent` Module**: This module provides functions for composing agents, such as `sequentialAgent` for running agents in sequence and `loopAgent` for running an agent in a loop.

## Example: Using a Tool

The framework allows you to define tools that the LLM can call to interact with the external world. Here is a complete example of how to define and use a tool to get the temperature for a given zipcode.

First, you need to define the data types for your tool's input and output. You'll need to provide `FromJSON` and `ToJSON` instances so the framework can handle the conversion to and from the JSON that the LLM uses.

```haskell
-- Input type for the temperature tool
newtype TemperatureInput = TemperatureInput {zipcode :: String}

instance FromJSON TemperatureInput where
  parseJSON = withObject "TemperatureInput" $ \v -> TemperatureInput <$> v .: "zipcode"

-- Output type for the temperature tool
data TemperatureOutput = TemperatureOutput
  { temperature :: Double,
    unit :: Text.Text
  }

instance ToJSON TemperatureOutput where
  toJSON (TemperatureOutput temp unit) = object ["temperature" .= temp, "unit" .= unit]
```

Next, write a Haskell function that performs the tool's action. This function should take the defined input type and return the output type within the `Agent` monad.

```haskell
-- The function that the tool will execute
getTemperature :: TemperatureInput -> Agent LLMAgentContext TemperatureOutput
getTemperature (TemperatureInput zip) = do
  -- In a real application, you would call an external API here.
  let temp = 72.0
      unit = "Fahrenheit"
  liftIO $ printf "--- Tool Execution: Getting temperature for zipcode %s ---\n" zip
  return $ TemperatureOutput {temperature = temp, unit = unit}
```

Now, you can define the tool itself. This includes metadata like its name and description, a JSON schema for its parameters, and the execution function you just wrote. The tool is wrapped in `AnyTool` to allow for lists of tools with different type signatures.

```haskell
-- The definition of the temperature tool
temperatureTool :: AnyTool LLMAgentContext
temperatureTool =
  AnyTool $
    Tool
      { name = "get_temperature",
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
```

Finally, add your tool to the `LLMAgentContext` and run an agent that supports tool execution, like `llmSingleTurnAgentWithToolExecution`.

```haskell
main :: IO ()
main = do
  key <- Environment.getEnv "OPENAI_KEY"
  client <- getClientEnv "https://api.openai.com"

  let llmSession =
        Session
          { chats = [UserMessage "Hello, I live in Seattle, 98101. What is the temperature right now?"],
            context =
              LLMAgentContext
                { apiKey = key,
                  clientEnv = client,
                  openAIModel = "gpt-4o-mini",
                  systemPrompt = "You are a helpful assistant. Use tools to answer questions.",
                  tools = [temperatureTool] -- Add your tool here
                }
          }

  -- Run an agent that can execute tools
  let agent = llmSingleTurnAgentWithToolExecution
  result <- runAgent agent llmSession

  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right (chat, s) -> do
      putStrLn "\n--- Agent's Final Response ---"
      print chat
      putStrLn "\n--- Full Chat History ---"
      print (chats s)
```