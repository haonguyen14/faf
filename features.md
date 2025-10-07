## Session management
A session maintains a chat history between the user and the AI model
A session enables the user to continuously converse with the AI model

## Simple agent
Agent simply response to chat message.
Is there a way to represent "agent" using functions. Why is OOP the only to make agent clean?

Ideas:
We should have an monad that maintains a final AgentContext and a stream of events. Maybe something like
`newtype AgentStreamM = (AgentContext, IO Stream Event)`

`agent :: AgentContext -> AgentStreamM`

Sequential agent is basically a composition of multiple agent

`sequentialAgent = agentA >=> agentB >=> agentC $ initialContext`

## Workflow agent
Agents can form a workflow. Support:
- Sequential workflow
- Loop workflow

Agents in the workflow can share states
