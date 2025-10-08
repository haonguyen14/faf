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

-   **`Agent` Monad**: The `Agent` monad is a state monad (`StateT (Session ctx) (ExceptT String IO)`) that manages the agent's state, including the conversation history (`chats`) and a context (`ctx`).

-   **`Session`**: The `Session` data type holds the current state of the agent, including the list of `Chat` messages and a user-defined `context`.

-   **`LLM` Module**: This module provides the `llmSingleTurnAgent` agent, which performs a single turn of conversation with the LLM. It also includes functions for converting between the framework's internal data types and the OpenAI API's data types.

-   **`Agent` Module**: This module provides functions for composing agents, such as `sequentialAgent` for running agents in sequence and `loopAgent` for running an agent in a loop.

## Example

Here is an example of how to create a simple agent that interacts with the LLM:

```haskell
import LLM
import Types
import Agent

main :: IO ()
main = do
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

  result <- runAgent llmSingleTurnAgent llmSession
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right (chat, _) -> print chat
```

This example creates an `LLMAgentContext` with a system prompt and a tool, and then runs the `llmSingleTurnAgent` agent with an initial user message. The agent will then call the OpenAI API and print the assistant's response.