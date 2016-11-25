{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Module      : Pinboard.Types
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Pinboard.Types
  ( PinboardEnv
  , PinboardT
  , runPinboardT
  , MonadPinboard
  , ExecLoggingT
  , PinboardConfig(..)
  , runConfigLoggingT
  , PinboardRequest(..)
  , ResultFormatType(..)
  , Param(..)
  , ParamsBS
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class (MonadIO)

import Control.Exception.Safe

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.IORef
import Network.HTTP.Client (Manager)

import Pinboard.Error
import Control.Monad.Logger

import Control.Applicative
import Prelude

------------------------------------------------------------------------------
type PinboardEnv = (PinboardConfig, Manager)

type PinboardT m a = ReaderT PinboardEnv (ExceptT PinboardError (LoggingT m)) a

runPinboardT
  :: (MonadIO m, MonadCatch m)
  => PinboardEnv -> PinboardT m a -> m (Either PinboardError a)
runPinboardT env@(config, _) f =
  runConfigLoggingT
    config
    (pinboardExceptionToEither (runExceptT (runReaderT f env)))

------------------------------------------------------------------------------
-- |Typeclass alias for the return type of the API functions (keeps the
-- signatures less verbose)
type MonadPinboard m = (Functor m, Applicative m, MonadIO m, MonadCatch m, MonadReader PinboardEnv m, MonadLogger m)

------------------------------------------------------------------------------
type ExecLoggingT = forall m. MonadIO m =>
                              forall a. LoggingT m a -> m a

data PinboardConfig = PinboardConfig
  { apiToken :: !ByteString
  , requestDelayMills :: !Int
  , lastRequestTime :: IORef UTCTime
  , doThreadDelay :: PinboardConfig -> IO ()
  , execLoggingT :: ExecLoggingT
  , filterLoggingT :: LogSource -> LogLevel -> Bool
  }

instance Show PinboardConfig where
  show (PinboardConfig a r _ _ _ _) =
    "{ apiToken = " ++ show a ++ ", requestDelayMills = " ++ show r ++ " }"

runConfigLoggingT :: PinboardConfig -> ExecLoggingT
runConfigLoggingT config =
  execLoggingT config . filterLogger (filterLoggingT config)

------------------------------------------------------------------------------
data PinboardRequest = PinboardRequest
  { requestPath :: !Text -- ^ url path of PinboardRequest
  , requestParams :: [Param] -- ^ Query Parameters of PinboardRequest
  } deriving (Show)

------------------------------------------------------------------------------
type ParamsBS = [(ByteString, ByteString)]

------------------------------------------------------------------------------
data ResultFormatType
  = FormatJson
  | FormatXml
  deriving (Show, Eq)

data Param
  = Format !ResultFormatType
  | Tag !Text
  | Tags !Text
  | Old !Text
  | New !Text
  | Count !Int
  | Start !Int
  | Results !Int
  | Url !Text
  | Date !Day
  | DateTime !UTCTime
  | FromDateTime !UTCTime
  | ToDateTime !UTCTime
  | Replace !Bool
  | Shared !Bool
  | ToRead !Bool
  | Description !Text
  | Extended !Text
  | Meta !Int
  deriving (Show, Eq)
