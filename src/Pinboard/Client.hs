{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections #-}

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
      -- * Connections
    , connOpenRaw
    , connOpen
    , connClose
    , connFail
     -- * JSON Streams
    ,parseJSONResponse
    ,decodeJSONResponse
     -- * Status Codes
    ,checkStatusCode
     -- * Error Helpers
    ,addErrMsg
    ,createParserErr
    ,httpStatusPinboardError
     -- * Client Dependencies
    , module Pinboard.Client.Error
    , module Pinboard.Client.Types
    , module Pinboard.Client.Util
    ) where


import Control.Exception          (catch, SomeException, try, bracket)
import Control.Monad.IO.Class     (MonadIO (liftIO))
import Control.Monad.Reader       (ask, runReaderT)
import Control.Monad.Trans.Either (runEitherT, hoistEither)
import Data.ByteString.Char8      (pack)
import Data.Monoid                ((<>))
import Data.Aeson                 (parseJSON, json', FromJSON, eitherDecodeStrict')
import Data.Aeson.Types           (parseEither)

import Network                    (withSocketsDo)
import Network.HTTP.Types         (urlEncode)

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method    as NHTM
import qualified Network.HTTP.Types.Status    as NHTS
import qualified Network.HTTP.Types.Header    as NHTH
import qualified Network.HTTP.Types.URI       as NHTU


import Pinboard.Client.Types
import Pinboard.Client.Error
import Pinboard.Client.Util

import qualified Data.ByteString             as S
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

import Control.Applicative
import Prelude


-- | Create a default PinboardConfig using the supplied apiToken
fromApiToken :: String -> PinboardConfig
fromApiToken token = PinboardConfig { debug = False, apiToken = pack token }

--------------------------------------------------------------------------------
-- | Execute computations in the Pinboard monad
runPinboard
    :: PinboardConfig
    -> Pinboard a
    -> IO (Either PinboardError a)
runPinboard config requests = 
  bracket connOpen connClose (either (connFail ConnectionFailure) go)
  where go conn = runReaderT (runEitherT requests) (config, conn) 
                  `catch` connFail UnknownErrorType

-- | Create a Pinboard value from a PinboardRequest w/ json deserialization
pinboardJson :: FromJSON a => PinboardRequest -> Pinboard a
pinboardJson req = do 
  (config, conn)  <- ask
  (_, result) <- liftIO $ sendPinboardRequest (ensureResultFormatType FormatJson req) config conn parseJSONResponse
  hoistEither result


--------------------------------------------------------------------------------

runPinboardSingleRaw
    :: PinboardConfig       
    -> PinboardRequest
    -> (Response LBS.ByteString -> a)
    -> IO (Either PinboardError a)
runPinboardSingleRaw config req handler = 
  bracket connOpen connClose (either (connFail ConnectionFailure) go)
    where go conn = (Right <$> sendPinboardRequest req config conn handler)
                    `catch` connFail UnknownErrorType

runPinboardSingleRawBS
    :: PinboardConfig       
    -> PinboardRequest
    -> IO (Either PinboardError LBS.ByteString)
runPinboardSingleRawBS config req = do
  res <- runPinboardSingleRaw config req id
  return $ do
    r <- res
    responseBody r <$ checkStatusCode (NHTS.statusCode (responseStatus r))

runPinboardSingleJson
    :: FromJSON a
    => PinboardConfig       
    -> PinboardRequest
    -> IO (Either PinboardError a)
runPinboardSingleJson config = runPinboard config . pinboardJson


--------------------------------------------------------------------------------

sendPinboardRequest
      :: PinboardRequest 
      -> PinboardConfig 
      -> Manager
      -> (Response LBS.ByteString -> a)
      -> IO a
sendPinboardRequest PinboardRequest{..} PinboardConfig{..} man handler = do
   let url = T.concat [ requestPath 
                      , "?" 
                      , T.decodeUtf8 $ paramsToByteString $ ("auth_token", urlEncode False apiToken) : encodeParams requestParams ]
   req <- buildReq $ T.unpack url
   res <- httpLbs req man
   return $ handler res

--------------------------------------------------------------------------------

buildReq ::  String -> IO Request
buildReq url = do
  req <- parseUrl $ "https://api.pinboard.in/v1/" <> url
  return $ req 
    { requestHeaders = [("User-Agent","pinboard.hs/0.7.5")]
    , checkStatus = \_ _ _ -> Nothing
    }

--------------------------------------------------------------------------------

parseJSONResponse
    :: (FromJSON a)
    => Response LBS.ByteString
    -> (Response LBS.ByteString, Either PinboardError a)
parseJSONResponse response = 
  (response, either (Left . addErrMsg (toText (responseBody response))) 
                    (const $ decodeJSONResponse (responseBody response)) 
                    (checkStatusCode (NHTS.statusCode (responseStatus response))))


decodeJSONResponse
    :: FromJSON a 
    => LBS.ByteString 
    -> Either PinboardError a
decodeJSONResponse s = 
  let r = eitherDecodeStrict' (LBS.toStrict s) 
  in either (Left . createParserErr . toText) Right r

--------------------------------------------------------------------------------

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


connOpenRaw :: IO Manager
connOpenRaw = withSocketsDo . newManager 
                $ managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings

connOpen :: IO (Either SomeException Manager)
connOpen = try connOpenRaw

connClose :: Either a Manager -> IO ()
connClose = either (const $ return ()) closeManager

connFail :: PinboardErrorType -> SomeException -> IO (Either PinboardError b)
connFail e msg = return $ Left $ PinboardError e (toText msg) Nothing Nothing Nothing


