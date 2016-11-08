{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

------------------------------------------------------------------------------
-- | 
-- Module      : Pinboard.Client
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
------------------------------------------------------------------------------
module Pinboard.Client
  ( fromApiToken
  , defaultPinboardConfig
   -- | The PinboardConfig provides authentication via apiToken
  , PinboardConfig(..)
   -- * Monadic
  , runPinboard
  , pinboardJson
   -- * Single
  , runPinboardSingleRaw
  , runPinboardSingleRawBS
  , runPinboardSingleJson
   -- * Sending
  , sendPinboardRequest
   -- *  Manager (http-client)
  , newMgr
  , mgrFail
   -- * JSON Handling
  , parseJSONResponse
  , decodeJSONResponse
   -- * Status Codes
  , checkStatusCodeResponse
  , checkStatusCode
   -- * Error Helpers
  , addErrMsg
  , createParserErr
  , httpStatusPinboardError
   -- * Client Dependencies
  , module X
  ) where

-- * Config
import Control.Monad.IO.Class
import Control.Monad.Reader

import Control.Exception.Safe
import Control.Monad.Error.Class (throwError)

import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, eitherDecodeStrict')

import Network (withSocketsDo)
import Network.HTTP.Types (urlEncode)
import Network.HTTP.Types.Status (statusCode)

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Control.Concurrent (threadDelay)
import Control.Monad.Logger

import Pinboard.Types as X
import Pinboard.Error as X
import Pinboard.Util as X
import Pinboard.Logging as X

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Applicative
import Prelude

-- | Create a default PinboardConfig using the supplied apiToken
fromApiToken :: String -> PinboardConfig
fromApiToken token =
  defaultPinboardConfig
  { apiToken = pack token
  }

defaultPinboardConfig :: PinboardConfig
defaultPinboardConfig =
  PinboardConfig
  { apiToken = mempty
  , requestDelayMills = 0
  , execLoggingT = runNullLoggingT
  , filterLoggingT = infoLevelFilter
  }

--------------------------------------------------------------------------------
-- | Execute computations in the Pinboard monad
runPinboard
  :: (MonadIO m, MonadCatch m, MonadErrorPinboard e)
  => PinboardConfig -> PinboardT m a -> m (e a)
runPinboard config f = do
  mgr <- liftIO newMgr
  eitherToMonadError <$> runPinboardT (config, mgr) f

-- | Create a Pinboard value from a PinboardRequest w/ json deserialization
pinboardJson
  :: (MonadPinboard m, FromJSON a)
  => PinboardRequest -> m a
pinboardJson req =
  logOnException logSrc $
  do logNST LevelInfo logSrc (toText req)
     env <- ask
     res <-
       liftIO $ sendPinboardRequest env (ensureResultFormatType FormatJson req)
     logNST LevelDebug logSrc (toText res)
     eitherToMonadThrow (parseJSONResponse res)
  where
    logSrc = "pinboardJson"

--------------------------------------------------------------------------------
runPinboardSingleRaw :: PinboardConfig
                     -> PinboardRequest
                     -> IO (Response LBS.ByteString)
runPinboardSingleRaw config req =
  runLogOnException logSrc config $
  do mgr <- liftIO newMgr
     logNST LevelInfo logSrc (toText req)
     res <- liftIO $ sendPinboardRequest (config, mgr) req
     logNST LevelDebug logSrc (toText res)
     return res
  where
    logSrc = "runPinboardSingleRaw"

runPinboardSingleRawBS
  :: (MonadErrorPinboard e)
  => PinboardConfig -> PinboardRequest -> IO (e LBS.ByteString)
runPinboardSingleRawBS config req = do
  res <- runPinboardSingleRaw config req
  case checkStatusCodeResponse res of
    Left e ->
      runConfigLoggingT
        config
        (do logNST LevelError logSrc (toText e)
            return (throwError e))
    Right _ -> return (return (responseBody res))
  where
    logSrc = "runPinboardSingleRawBS"

runPinboardSingleJson
  :: (MonadErrorPinboard e, FromJSON a)
  => PinboardConfig -> PinboardRequest -> IO (e a)
runPinboardSingleJson config = runPinboard config . pinboardJson

--------------------------------------------------------------------------------
sendPinboardRequest :: PinboardEnv
                    -> PinboardRequest
                    -> IO (Response LBS.ByteString)
sendPinboardRequest (PinboardConfig {..}, mgr) PinboardRequest {..} = do
  let encodedParams = ("auth_token", urlEncode False apiToken) : encodeParams requestParams
      paramsText = T.decodeUtf8 (paramsToByteString encodedParams)
      url = T.unpack $ T.concat [requestPath, "?", paramsText]
  when (requestDelayMills > 0) $ threadDelay (requestDelayMills * 1000)
  req <- buildReq url
  httpLbs req mgr

--------------------------------------------------------------------------------
buildReq :: String -> IO Request
buildReq url = do
  req <- parseRequest $ "https://api.pinboard.in/v1/" <> url
  return $
    setRequestIgnoreStatus $
    req
    { requestHeaders = [("User-Agent", "pinboard.hs/0.9.11")]
    }

--------------------------------------------------------------------------------
parseJSONResponse
  :: (MonadErrorPinboard e, FromJSON a)
  => Response LBS.ByteString -> e a
parseJSONResponse response =
  either
    (throwError . addErrMsg (toText (responseBody response)))
    (const $ decodeJSONResponse (responseBody response))
    (checkStatusCodeResponse response)

decodeJSONResponse
  :: (MonadErrorPinboard e, FromJSON a)
  => LBS.ByteString -> e a
decodeJSONResponse s =
  let r = eitherDecodeStrict' (LBS.toStrict s)
  in either (throwError . createParserErr . T.pack) return r

--------------------------------------------------------------------------------
checkStatusCodeResponse
  :: MonadErrorPinboard e
  => Response a -> e ()
checkStatusCodeResponse = checkStatusCode . statusCode . responseStatus

checkStatusCode
  :: MonadErrorPinboard e
  => Int -> e ()
checkStatusCode =
  \case
    200 -> return ()
    400 -> httpStatusPinboardError BadRequest
    401 -> httpStatusPinboardError UnAuthorized
    402 -> httpStatusPinboardError RequestFailed
    403 -> httpStatusPinboardError Forbidden
    404 -> httpStatusPinboardError NotFound
    429 -> httpStatusPinboardError TooManyRequests
    c
      | c >= 500 -> httpStatusPinboardError PinboardServerError
    _ -> httpStatusPinboardError UnknownHTTPCode

--------------------------------------------------------------------------------
httpStatusPinboardError
  :: MonadErrorPinboard e
  => PinboardErrorHTTPCode -> e a
httpStatusPinboardError err =
  throwError
    defaultPinboardError
    { errorType = HttpStatusFailure
    , errorHTTP = Just err
    }

addErrMsg :: T.Text -> PinboardError -> PinboardError
addErrMsg msg err =
  err
  { errorMsg = msg
  }

createParserErr :: T.Text -> PinboardError
createParserErr msg = PinboardError ParseFailure msg Nothing Nothing Nothing

--------------------------------------------------------------------------------
newMgr :: IO Manager
newMgr =
  withSocketsDo . newManager $ managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings

mgrFail
  :: (Monad m, MonadErrorPinboard e)
  => PinboardErrorType -> SomeException -> m (e b)
mgrFail e msg =
  return $ throwError $ PinboardError e (toText msg) Nothing Nothing Nothing
