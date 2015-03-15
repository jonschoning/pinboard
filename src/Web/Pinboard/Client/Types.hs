{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Web.Pinboard.Client.Types
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Pinboard.Client.Types
  ( Pinboard
  , PinboardRequest (..)
  , PinboardConfig  (..)
  , Param (..)
  , ParamsBS
  ) where

import Control.Monad.Reader       (ReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.ByteString            (ByteString)
import Data.Text                  (Text)
import Network.Http.Client        (Connection)
import Web.Pinboard.Client.Error  (PinboardError (..))
import Data.Time.Calendar(Day)
import Data.Time.Clock(UTCTime)

------------------------------------------------------------------------------

type Pinboard = EitherT PinboardError (ReaderT (PinboardConfig, Connection) IO)

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

data Param = Format Text
           | Tag Text
           | Old Text
           | New Text
           | Count Int
           | Url Text
           | Date Day
           | DateTime UTCTime
      deriving (Show, Eq)

