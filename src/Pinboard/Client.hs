{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
      -- | The PinboardConfig provides authentication via apiToken
    , PinboardConfig       (..)
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
    ,parseJSONResponse
    ,decodeJSONResponse
     -- * Status Codes
    ,checkStatusCodeResponse
    ,checkStatusCode
     -- * Error Helpers
    ,addErrMsg
    ,createParserErr
    ,httpStatusPinboardError
     -- * Client Dependencies
    , module X
    ) where


import Control.Exception          (catch, SomeException)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except

import Data.ByteString.Char8      (pack)
import Data.Monoid                ((<>))
import Data.Aeson                 (FromJSON, eitherDecodeStrict')


import Network                    (withSocketsDo)
import Network.HTTP.Types         (urlEncode)
import Network.HTTP.Types.Status  (statusCode)

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS


import Pinboard.Types as X
import Pinboard.Error as X
import Pinboard.Util as X

import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

import Control.Applicative
import Prelude


-- | Create a default PinboardConfig using the supplied apiToken
fromApiToken :: String -> PinboardConfig
fromApiToken token = PinboardConfig { apiToken = pack token }

--------------------------------------------------------------------------------
-- | Execute computations in the Pinboard monad
runPinboard
    :: MonadIO m
    => PinboardConfig
    -> PinboardT m a
    -> m (Either PinboardError a)
runPinboard config f = newMgr >>= go
    where go mgr = runPinboardT (config, mgr) f


-- | Create a Pinboard value from a PinboardRequest w/ json deserialization
pinboardJson 
    :: (MonadPinboard m, FromJSON a) 
    => PinboardRequest 
    -> m a
pinboardJson req = do 
  env <- ask
  res <- sendPinboardRequest env (ensureResultFormatType FormatJson req) 
  parseJSONResponse res

--------------------------------------------------------------------------------

runPinboardSingleRaw
    :: MonadIO m
    => PinboardConfig       
    -> PinboardRequest
    -> m (Either PinboardError (Response LBS.ByteString))
runPinboardSingleRaw config req = liftIO $ newMgr >>= go
  where go mgr = (Right <$> sendPinboardRequest (config, mgr) req)
                    `catch` mgrFail UnknownErrorType

runPinboardSingleRawBS
    :: MonadIO m
    => PinboardConfig       
    -> PinboardRequest
    -> m (Either PinboardError LBS.ByteString)
runPinboardSingleRawBS config req = do
  res <- runPinboardSingleRaw config req
  return $ do
    r <- res
    responseBody r <$ checkStatusCodeResponse r

runPinboardSingleJson
    :: (MonadIO m, FromJSON a)
    => PinboardConfig       
    -> PinboardRequest
    -> m (Either PinboardError a)
runPinboardSingleJson config = runPinboard config . pinboardJson


--------------------------------------------------------------------------------

sendPinboardRequest
      :: MonadIO m
      => PinboardEnv
      -> PinboardRequest 
      -> m (Response LBS.ByteString)
sendPinboardRequest (PinboardConfig{..}, mgr) PinboardRequest{..} = do
   let url = T.concat [ requestPath 
                      , "?" 
                      , T.decodeUtf8 $ paramsToByteString $ ("auth_token", urlEncode False apiToken) : encodeParams requestParams ]
   req <- buildReq $ T.unpack url
   liftIO $ httpLbs req mgr

--------------------------------------------------------------------------------

buildReq :: MonadIO m => String -> m Request
buildReq url = do
  req <- liftIO $ parseUrl $ "https://api.pinboard.in/v1/" <> url
  return $ req 
    { requestHeaders = [("User-Agent","pinboard.hs/0.9.6")]
    , checkStatus = \_ _ _ -> Nothing
    }

--------------------------------------------------------------------------------

parseJSONResponse
    :: (MonadError PinboardError m, FromJSON a)
    => Response LBS.ByteString
    -> m a
parseJSONResponse response = 
  either (throwError . addErrMsg (toText (responseBody response))) 
         (const $ decodeJSONResponse (responseBody response)) 
         (checkStatusCodeResponse response)


decodeJSONResponse
    :: (MonadError PinboardError m, FromJSON a) 
    => LBS.ByteString 
    -> m a
decodeJSONResponse s = 
  let r = eitherDecodeStrict' (LBS.toStrict s) 
  in either (throwError . createParserErr . toText) return r

--------------------------------------------------------------------------------

checkStatusCodeResponse :: Response a -> Either PinboardError ()
checkStatusCodeResponse = checkStatusCode . statusCode . responseStatus

checkStatusCode :: Int -> Either PinboardError ()
checkStatusCode = \case
  200 -> Right ()
  400 -> httpStatusPinboardError BadRequest
  401 -> httpStatusPinboardError UnAuthorized
  402 -> httpStatusPinboardError RequestFailed
  403 -> httpStatusPinboardError Forbidden
  404 -> httpStatusPinboardError NotFound
  429 -> httpStatusPinboardError TooManyRequests
  c | c >= 500 -> httpStatusPinboardError PinboardServerError
  _   -> httpStatusPinboardError UnknownHTTPCode

--------------------------------------------------------------------------------

httpStatusPinboardError :: PinboardErrorHTTPCode -> Either PinboardError a
httpStatusPinboardError err = Left $ defaultPinboardError 
  { errorType = HttpStatusFailure
  , errorHTTP = Just err }

addErrMsg :: T.Text -> PinboardError -> PinboardError
addErrMsg msg err = err {errorMsg = msg}

createParserErr :: T.Text -> PinboardError
createParserErr msg = PinboardError ParseFailure msg Nothing Nothing Nothing 

--------------------------------------------------------------------------------


newMgr :: MonadIO m => m Manager
newMgr = liftIO $ withSocketsDo . newManager 
                $ managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings

mgrFail :: MonadIO m => PinboardErrorType -> SomeException -> m (Either PinboardError b)
mgrFail e msg = return $ Left $ PinboardError e (toText msg) Nothing Nothing Nothing


