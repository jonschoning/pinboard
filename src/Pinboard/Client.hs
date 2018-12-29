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
  (
    -- * Config
    fromApiToken
  , defaultPinboardConfig
   -- | The PinboardConfig provides authentication via apiToken
  , PinboardConfig(..)
   -- * Monadic
  , runPinboard
  , runPinboardE
  , pinboardJson
   -- * Single
  , runPinboardSingleRaw
  , runPinboardSingleRawBS
  , runPinboardSingleJson
   -- * Sending
  , sendPinboardRequest
   -- * Delaying
  , requestThreadDelay
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

import Control.Monad.IO.Class
import Control.Monad.Reader

import Control.Monad.IO.Unlift
import UnliftIO.Exception

import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, eitherDecodeStrict')

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

import Paths_pinboard (version)
import Data.Version (showVersion)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.IO (unsafePerformIO)
import Data.IORef
import Data.Time.Clock
import Data.Time.Calendar
import Data.Bifunctor
import Data.Function

import Control.Applicative
import Prelude

-- | Create a default PinboardConfig using the supplied apiToken (ex: "use:ABCDEF0123456789ABCD")
fromApiToken :: String -> PinboardConfig
fromApiToken token =
  defaultPinboardConfig
  { apiToken = pack token
  }

defaultPinboardConfig :: PinboardConfig
defaultPinboardConfig =
  PinboardConfig
  { apiToken = mempty
  , maxRequestRateMills = 0
  , execLoggingT = runNullLoggingT
  , filterLoggingT = infoLevelFilter
  , lastRequestTime =
    unsafePerformIO $ newIORef (UTCTime (ModifiedJulianDay 0) 0)
  , doThreadDelay = Pinboard.Client.requestThreadDelay
  }
{-# NOINLINE defaultPinboardConfig #-}

--------------------------------------------------------------------------------
-- | Execute computations in the Pinboard monad
runPinboard
  :: MonadUnliftIO m
  => PinboardConfig -> PinboardT m a -> m a
runPinboard config f = liftIO newMgr >>= \mgr -> runPinboardE (config, mgr) f

-- | Execute computations in the Pinboard monad (with specified http Manager)
runPinboardE
  :: MonadUnliftIO m
  => PinboardEnv -> PinboardT m a -> m a
runPinboardE (config, mgr) f =
  runPinboardT (config, mgr) f

-- | Create a Pinboard value from a PinboardRequest w/ json deserialization
pinboardJson
  :: (MonadPinboard m, FromJSON a)
  => PinboardRequest -> m (Either PinboardError a)
pinboardJson req =
  logOnException logSrc $
  do logNST LevelInfo logSrc (toText req)
     env <- ask
     res <-
       liftIO $ sendPinboardRequest env (ensureResultFormatType FormatJson req)
     logNST LevelDebug logSrc (toText res)
     pure (parseJSONResponse res)
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
  :: 
  PinboardConfig -> PinboardRequest -> IO (Either PinboardError LBS.ByteString)
runPinboardSingleRawBS config req = do
  res <- runPinboardSingleRaw config req
  case checkStatusCodeResponse res of
    Left e -> logErrorAndThrow e
    Right _ -> (return . return) (responseBody res)
  where
    logSrc = "runPinboardSingleRawBS"
    logErrorAndThrow e =
      runConfigLoggingT config $
      do logNST LevelError logSrc (toText e)
         return (Left e)

runPinboardSingleJson
  :: FromJSON a
  => PinboardConfig -> PinboardRequest -> IO (Either PinboardError a)
runPinboardSingleJson config = runPinboard config . pinboardJson

--------------------------------------------------------------------------------
sendPinboardRequest :: PinboardEnv
                    -> PinboardRequest
                    -> IO (Response LBS.ByteString)
sendPinboardRequest (cfg@PinboardConfig {..}, mgr) PinboardRequest {..} = do
  let encodedParams = ("auth_token", urlEncode False apiToken) : encodeParams requestParams
      paramsText = T.decodeUtf8 (paramsToByteString encodedParams)
      url = T.unpack $ T.concat [requestPath, "?", paramsText]
  req <- buildReq url
  doThreadDelay cfg
  httpLbs req mgr

--------------------------------------------------------------------------------

-- | delays the thread, if the time since the previous request is less than the configured maxRequestRateMills 
requestThreadDelay :: PinboardConfig -> IO ()
requestThreadDelay cfg@PinboardConfig {..} = do
  currentTime <- getCurrentTime
  lastTime <- readIORef lastRequestTime
  let elapsedtime = diffUTCTime currentTime lastTime
      delaytime = max 0 (maxRequestRateSecs - elapsedtime)
  when (delaytime > 0) $
    do runConfigLoggingT cfg $
         let logTxt =
               "DELAY " <> ", lastTime: " <> toText lastTime <>
               ", maxRequestRateSecs: " <>
               toText maxRequestRateSecs <>
               ", elapsedTime: " <>
               toText elapsedtime <>
               ", delayTime: " <>
               toText delaytime
         in logNST LevelInfo "requestThreadDelay" logTxt
       threadDelay (floor (delaytime * 1000000))
  currentTime' <- getCurrentTime
  writeIORef lastRequestTime currentTime'
  where
    maxRequestRateSecs = fromInteger (toInteger maxRequestRateMills) / 1000

--------------------------------------------------------------------------------
buildReq :: String -> IO Request
buildReq url = do
  req <- parseRequest $ "https://api.pinboard.in/v1/" <> url
  return $
    setRequestIgnoreStatus $
    req
    { requestHeaders = [("User-Agent", "pinboard.hs/" <> pack (showVersion version))]
    }

--------------------------------------------------------------------------------
parseJSONResponse
  :: FromJSON a
  => Response LBS.ByteString -> Either PinboardError a
parseJSONResponse response =
  checkStatusCodeResponse response
  *> decodeJSONResponse (responseBody response)

decodeJSONResponse
  :: FromJSON a
  => LBS.ByteString -> Either PinboardError a
decodeJSONResponse s =
  let r = eitherDecodeStrict' (LBS.toStrict s)
  in either (Left . createParserErr . T.pack) return r

--------------------------------------------------------------------------------
checkStatusCodeResponse
  :: Response LBS.ByteString -> Either PinboardError ()
checkStatusCodeResponse resp =
  (checkStatusCode . statusCode . responseStatus) resp
  & (first . addErrMsg . toText . responseBody) resp

checkStatusCode
  :: Int -> Either PinboardError ()
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
  :: PinboardErrorHTTPCode -> Either PinboardError a
httpStatusPinboardError err =
  Left
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
  newManager $ managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings

mgrFail
  :: (Monad m)
  => PinboardErrorType -> SomeException -> m (Either PinboardError b)
mgrFail e msg =
  return $ Left $ PinboardError e (toText msg) Nothing Nothing Nothing
