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
import Control.Monad.Trans.Reader (runReaderT)

import UnliftIO

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client (Manager)

import Control.Monad.Logger

import Prelude

------------------------------------------------------------------------------
type PinboardEnv = (PinboardConfig, Manager)

type PinboardT m a = ReaderT PinboardEnv (LoggingT m) a

runPinboardT
  :: MonadUnliftIO m
  => PinboardEnv -> PinboardT m a -> m a
runPinboardT env@(config, _) f =
  runConfigLoggingT
    config
    (runReaderT f env)

------------------------------------------------------------------------------
-- |Typeclass alias for the return type of the API functions (keeps the
-- signatures less verbose)
type MonadPinboard m = (MonadUnliftIO m, MonadReader PinboardEnv m, MonadLogger m)

------------------------------------------------------------------------------
type ExecLoggingT = forall m. MonadIO m =>
                              forall a. LoggingT m a -> m a

data PinboardConfig = PinboardConfig
  { apiToken :: !ByteString
  , maxRequestRateMills :: !Int
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
