{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types
  ( Chat (..),
    FunctionCall (..),
    FunctionCallParams (..),
    Session (..),
    Agent (..),
    module Control.Monad.Error.Class,
    module Control.Monad.State.Class,
    module Control.Monad.IO.Class,
  )
where

import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Aeson (Value)
import qualified Data.Text as Text

data Chat
  = SystemMessage Text.Text
  | UserMessage Text.Text
  | AssistantMessage {text :: Maybe Text.Text, functionCalls :: [FunctionCall]}
  deriving (Show)

data FunctionCall = FunctionCall
  { id :: String,
    function :: FunctionCallParams
  }
  deriving (Show)

data FunctionCallParams = FunctionCallParams
  { name :: String,
    arguments :: Value
  }
  deriving (Show)

data Session a = Session {chats :: [Chat], context :: a}

newtype Agent ctx a = Agent {runAgent :: Session ctx -> IO (Either String (a, Session ctx))}

instance Functor (Agent ctx) where
  fmap :: (a -> b) -> Agent ctx a -> Agent ctx b
  fmap f agent = Agent $ \s -> do
    result <- runAgent agent s
    case result of
      Left err -> return $ Left err
      Right (a, s') -> return $ Right (f a, s')

instance Applicative (Agent ctx) where
  pure :: a -> Agent ctx a
  pure a = Agent $ \s -> return $ Right (a, s)

  (<*>) :: Agent ctx (a -> b) -> Agent ctx a -> Agent ctx b
  ff <*> agent = Agent $ \s -> do
    r1 <- runAgent ff s
    case r1 of
      Left err -> return $ Left err
      Right (f, s') -> do
        r2 <- runAgent agent s'
        case r2 of
          Left err -> return $ Left err
          Right (a, s'') -> return $ Right (f a, s'')

instance Monad (Agent ctx) where
  return :: a -> Agent ctx a
  return = pure

  (>>=) :: Agent ctx a -> (a -> Agent ctx b) -> Agent ctx b
  agent >>= f = Agent $ \s -> do
    r <- runAgent agent s
    case r of
      Left err -> return $ Left err
      Right (a, s') -> runAgent (f a) s'

instance MonadState (Session ctx) (Agent ctx) where
  get :: Agent ctx (Session ctx)
  get = Agent $ \s -> return $ Right (s, s)

  put :: Session ctx -> Agent ctx ()
  put s = Agent $ \_ -> return $ Right ((), s)

instance MonadError String (Agent ctx) where
  throwError :: String -> Agent ctx a
  throwError err = Agent $ \_ -> return $ Left err

  catchError :: Agent ctx a -> (String -> Agent ctx a) -> Agent ctx a
  catchError agent _ = agent

instance MonadIO (Agent ctx) where
  liftIO :: IO a -> Agent ctx a
  liftIO io = Agent $ \s -> do
    a <- io
    return $ Right (a, s)
