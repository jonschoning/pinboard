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
  ( MonadErrorPinboard
  , defaultPinboardError
  , pinboardExceptionToEither
  , pinboardExceptionToMonadError
  , exceptionToMonadErrorPinboard
  , tryMonadError
  , eitherToMonadError
  , eitherToMonadThrow
  , PinboardErrorHTTPCode(..)
  , PinboardErrorType(..)
  , PinboardErrorCode(..)
  , PinboardError(..)
  ) where

import Data.Text (Text, pack)

import Data.Monoid
import Prelude

import Control.Exception.Safe
import Control.Monad.Error.Class (MonadError, throwError)

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

type MonadErrorPinboard m = MonadError PinboardError m

defaultPinboardError :: PinboardError
defaultPinboardError = PinboardError UnknownErrorType mempty Nothing Nothing Nothing

pinboardExceptionToEither
  :: MonadCatch m
  => m (Either PinboardError a) -> m (Either PinboardError a)
pinboardExceptionToEither = handle (\(e :: PinboardError) -> return (Left e))

tryMonadError
  :: (Exception e, MonadCatch m, MonadError e r)
  => m a -> m (r a)
tryMonadError a = eitherToMonadError <$> try a

pinboardExceptionToMonadError
  :: (MonadCatch m, MonadErrorPinboard e)
  => m (e a) -> m (e a)
pinboardExceptionToMonadError =
  handle (\(e :: PinboardError) -> return (throwError e))

exceptionToMonadErrorPinboard
  :: (MonadCatch m, MonadErrorPinboard e)
  => m (e a) -> m (e a)
exceptionToMonadErrorPinboard =
  handle
    (\(e :: SomeException) ->
        return $
        throwError $
        defaultPinboardError
        { errorMsg = (pack . show) e
        })

eitherToMonadError
  :: MonadError e m
  => Either e a -> m a
eitherToMonadError = either throwError return

eitherToMonadThrow
  :: (Exception e, MonadThrow m)
  => Either e a -> m a
eitherToMonadThrow = either throw return
