{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides high-level combinators for creating and running agents.
-- It defines how agents can be composed into sequences or loops to form complex behaviors.
module Agent
  ( sequential,
    loop,
    quitAfterNTerminator,
    appendOutput,
    Streamer (runStreamer),
  )
where

import Types

-- | Represents an agent that produces a stream of values 'a'.
-- This newtype is used to provide cleaner type signatures for functions that return streaming agents.
newtype Streamer ctx a = Streamer {runStreamer :: Agent ctx (AgentStream' ctx a)}

-- | A higher-order agent that takes another agent, runs it, and appends its 'Chat' output to the session history.
appendOutput :: Agent ctx Chat -> Agent ctx Chat
appendOutput a = do
  result <- a
  modify $ \s -> s {chats = chats s ++ [result]}
  return result

-- | Creates a streaming agent that runs a list of agents in sequence.
-- Each agent is executed one after the other, and their results are returned as a stream.
sequential :: [Agent ctx Chat] -> Streamer ctx Chat
sequential [] = Streamer $ return Nil
sequential (a : as) = Streamer $ do
  c <- appendOutput a
  return . Cons c . runStreamer $ sequential as

-- | Creates a streaming agent that runs a 'looper' agent in a loop until a 'terminator' agent signals to stop.
-- The 'terminator' agent is run before each iteration. If it returns 'Nothing', the 'looper' is run and its result is added to the stream.
-- If it returns 'Just ()', the loop terminates.
loop :: Agent ctx (Maybe ()) -> Agent ctx Chat -> Streamer ctx Chat
loop terminator looper = Streamer $ do
  result <- terminator
  case result of
    Just _ -> return Nil
    Nothing -> do
      c <- appendOutput looper
      return . Cons c . runStreamer $ loop terminator looper

-- | A terminator agent that signals to stop after a certain number of chats have occurred in the session.
quitAfterNTerminator :: Int -> Agent ctx (Maybe ())
quitAfterNTerminator n = do
  s <- get
  if length (chats s) >= n
    then return $ Just ()
    else return Nothing
