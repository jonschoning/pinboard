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
  ( Pinboard
  , MonadPinboard
  , PinboardRequest (..)
  , PinboardConfig  (..)
  , ResultFormatType (..)
  , Param (..)
  , ParamsBS
  ) where

import Control.Monad.Reader       (ReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.ByteString            (ByteString)
import Data.Text                  (Text)
import Network.HTTP.Client        (Manager)
import Pinboard.Client.Error  (PinboardError (..))
import Data.Time.Calendar(Day)
import Data.Time.Clock(UTCTime)
import Control.Monad.Reader.Class(MonadReader)
import Control.Monad.Error.Class(MonadError)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Except(ExceptT)
import Data.Functor.Identity(Identity)

------------------------------------------------------------------------------

type Pinboard a = ExceptT PinboardError (ReaderT (PinboardConfig, Manager) IO) a

-- |Typeclass alias for the return type of the API functions (keeps the
-- signatures less verbose)
type MonadPinboard m =
  ( Monad m
  , MonadIO m
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

