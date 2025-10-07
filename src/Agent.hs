{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent (repeatAgent, sequentialAgent, loopAgent, quitAfterNTerminator) where

import Data.Foldable (traverse_)
import qualified Data.Text as Text
import Types

appendOutput :: Agent ctx Chat -> Agent ctx ()
appendOutput a = do
  result <- a
  -- appending is slow for a list, in the future use Data.Sequence
  modify $ \s -> if Text.null result then s else s {chats = chats s ++ [result]}

sequentialAgent :: [Agent ctx Chat] -> Agent ctx Chat
sequentialAgent agents = do
  traverse_ appendOutput (init agents)
  last agents

loopAgent :: Agent ctx (Maybe ()) -> Agent ctx Chat -> Agent ctx ()
loopAgent terminator looper = do
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
    [] -> throwError "Chat history is empty"
    m : _ -> return m

quitAfterNTerminator :: Int -> Agent ctx (Maybe ())
quitAfterNTerminator n = do
  s <- get
  if length (chats s) >= n
    then return $ Just ()
    else return Nothing
