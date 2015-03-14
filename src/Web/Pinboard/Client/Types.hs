{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Data                  (Data)
import Data.Text                  (Text)
import Data.Typeable              (Typeable)
import Network.Http.Client        (Connection)
import Web.Pinboard.Client.Error  (PinboardError (..))

------------------------------------------------------------------------------

type Pinboard = EitherT PinboardError (ReaderT (PinboardConfig, Connection) IO)

------------------------------------------------------------------------------

data PinboardRequest = PinboardRequest
    { path    :: Text   -- ^ url path of PinboardRequest
    , queryParams :: [Param] -- ^ Query Parameters of PinboardRequest
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
           | Count Int
      deriving (Show, Eq, Data, Typeable)

