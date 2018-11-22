{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Pinboard.Error
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Pinboard.Error
  ( defaultPinboardError
  , PinboardErrorHTTPCode(..)
  , PinboardErrorType(..)
  , PinboardErrorCode(..)
  , PinboardError(..)
  ) where

import Data.Text (Text)

import Data.Monoid
import Prelude

import UnliftIO

------------------------------------------------------------------------------
data PinboardErrorHTTPCode
  = BadRequest -- ^ 400
  | UnAuthorized -- ^ 401
  | RequestFailed -- ^ 402
  | Forbidden -- ^ 403
  | NotFound -- ^ 404
  | TooManyRequests -- ^ 429
  | PinboardServerError -- ^ (>=500)
  | UnknownHTTPCode -- ^ All other codes
  deriving (Show)

------------------------------------------------------------------------------
data PinboardErrorType
  = ConnectionFailure
  | HttpStatusFailure
  | ParseFailure
  | UnknownErrorType
  deriving (Eq, Show)

------------------------------------------------------------------------------
data PinboardErrorCode =
  UnknownError
  deriving (Show)

------------------------------------------------------------------------------
data PinboardError = PinboardError
  { errorType :: PinboardErrorType
  , errorMsg :: !Text
  , errorCode :: Maybe PinboardErrorCode
  , errorParam :: Maybe Text
  , errorHTTP :: Maybe PinboardErrorHTTPCode
  } deriving (Show)

instance Exception PinboardError

defaultPinboardError :: PinboardError
defaultPinboardError = PinboardError UnknownErrorType mempty Nothing Nothing Nothing
