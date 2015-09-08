{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Pinboard.Client.Types
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Pinboard.Client.Types
  ( PinboardT
  , runPinboardT
  , MonadPinboard
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
import Control.Monad.Error.Class  (MonadError)
import Control.Monad.IO.Class     (MonadIO)

import Data.ByteString            (ByteString)
import Data.Text                  (Text)
import Data.Time.Calendar(Day)
import Data.Time.Clock(UTCTime)
import Network.HTTP.Client        (Manager)

import Pinboard.Client.Error  (PinboardError (..))

------------------------------------------------------------------------------

type PinboardT m a = ReaderT (PinboardConfig, Manager) (ExceptT PinboardError m) a

runPinboardT 
  :: PinboardConfig 
  -> Manager 
  -> PinboardT m a
  -> m (Either PinboardError a)
runPinboardT config mgr f = runExceptT $ runReaderT f (config, mgr) 

-- |Typeclass alias for the return type of the API functions (keeps the
-- signatures less verbose)
type MonadPinboard m =
  ( MonadIO m
  , MonadReader (PinboardConfig, Manager) m
  , MonadError PinboardError m
  )

------------------------------------------------------------------------------

data PinboardRequest = PinboardRequest
    { requestPath    :: Text   -- ^ url path of PinboardRequest
    , requestParams :: [Param] -- ^ Query Parameters of PinboardRequest
    } deriving Show

------------------------------------------------------------------------------
data PinboardConfig = PinboardConfig
    { apiToken :: ByteString
    , debug :: Bool
    } deriving Show

------------------------------------------------------------------------------

type ParamsBS = [(ByteString, ByteString)]

------------------------------------------------------------------------------

data ResultFormatType = FormatJson | FormatXml
      deriving (Show, Eq)

data Param = Format ResultFormatType
           | Tag Text
           | Tags Text
           | Old Text
           | New Text
           | Count Int
           | Start Int
           | Results Int
           | Url Text
           | Date Day
           | DateTime UTCTime
           | FromDateTime UTCTime
           | ToDateTime UTCTime
           | Replace Bool
           | Shared Bool
           | ToRead Bool
           | Description Text
           | Extended Text
           | Meta Int
      deriving (Show, Eq)

