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
  , mkParams
  , mkConfig
  ) where

import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.Trans.Either (EitherT)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Network.Http.Client        (Connection)
import           Web.Pinboard.Client.Error    (PinboardError (..))
import Data.Monoid(mempty)

------------------------------------------------------------------------------

type Pinboard = EitherT PinboardError (ReaderT (PinboardConfig, Connection) IO)

------------------------------------------------------------------------------
type Params = [(ByteString, ByteString)]

------------------------------------------------------------------------------
data PinboardRequest = PinboardRequest
    { endpoint    :: Text   -- ^ Endpoint of PinboardRequest
    , queryParams :: Params -- ^ Query Parameters of PinboardRequest
    } deriving Show

------------------------------------------------------------------------------
data PinboardConfig = PinboardConfig
    { apiToken :: ByteString
    , debug :: Bool
    } deriving Show

------------------------------------------------------------------------------

mkParams :: Params
mkParams = mempty

mkConfig :: PinboardConfig
mkConfig = PinboardConfig { debug = False, apiToken = mempty }
