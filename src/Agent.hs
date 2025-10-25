{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent (repeatAgent, sequentialAgent, loopAgent, quitAfterNTerminator, appendOutput) where

import Data.Foldable (traverse_)
import Types

appendOutput :: Agent ctx Chat -> Agent ctx Chat
appendOutput a = do
  result <- a
  modify $ \s -> s {chats = chats s ++ [result]}
  return result

sequentialAgent :: [Agent ctx Chat] -> Agent ctx Chat
sequentialAgent agents = do
  traverse_ appendOutput (init agents)
  last agents

loopAgent :: Agent ctx (Maybe ()) -> Agent ctx Chat -> Agent ctx ()
loopAgent terminator looper = do
  s <- get
  liftIO . print $ s

  result <- terminator
  case result of
    Just termSignal -> return termSignal
    Nothing -> do
      _ <- appendOutput looper
      loopAgent terminator looper

-- this is a contrived example

repeatAgent :: Agent ctx Chat
repeatAgent = do
  s <- get
  case chats s of
    [] -> throwError ("Chat history is empty", s)
    m : _ -> return m

quitAfterNTerminator :: Int -> Agent ctx (Maybe ())
quitAfterNTerminator n = do
  s <- get
  if length (chats s) >= n
    then return $ Just ()
    else return Nothing
