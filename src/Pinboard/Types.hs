{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , MonadErrorPinboard
  , PinboardRequest (..)
  , PinboardConfig  (..)
  , ResultFormatType (..)
  , Param (..)
  , ParamsBS
  ) where

import Control.Monad.Reader       (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class     (MonadIO)

import Data.ByteString            (ByteString)
import Data.Text                  (Text)
import Data.Time.Calendar(Day)
import Data.Time.Clock(UTCTime)
import Network.HTTP.Client        (Manager)

import Pinboard.Error

import Control.Applicative
import Control.Exception.Safe
import Prelude

------------------------------------------------------------------------------

type PinboardEnv = (PinboardConfig, Manager)

type PinboardT m a = ReaderT PinboardEnv (ExceptT PinboardError m) a

runPinboardT 
  :: MonadCatch m
  => PinboardEnv
  -> PinboardT m a
  -> m (Either PinboardError a)
runPinboardT e f = pinboardExceptionToEither (runExceptT (runReaderT f e))

-- |Typeclass alias for the return type of the API functions (keeps the
-- signatures less verbose)
type MonadPinboard m =
  ( Functor m
  , Applicative m
  , MonadIO m
  , MonadCatch m
  , MonadReader PinboardEnv m
  )

------------------------------------------------------------------------------

data PinboardRequest = PinboardRequest
    { requestPath    :: !Text   -- ^ url path of PinboardRequest
    , requestParams :: [Param] -- ^ Query Parameters of PinboardRequest
    } deriving Show

------------------------------------------------------------------------------
data PinboardConfig = PinboardConfig
    { apiToken :: !ByteString
    } deriving Show

------------------------------------------------------------------------------

type ParamsBS = [(ByteString, ByteString)]

------------------------------------------------------------------------------

data ResultFormatType = FormatJson | FormatXml
      deriving (Show, Eq)

data Param = Format !ResultFormatType
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

