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
    , sendPinboardRequestBS
      -- * Connections
    , connOpenRaw
    , connOpen
    , connClose
    , connFail
     -- * JSON Streams
    ,parseJSONResponseStream
    ,parseJSONFromStream
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


import Control.Applicative        ((<$>))
import Control.Exception          (catch, SomeException, try, bracket)
import Control.Monad.IO.Class     (MonadIO (liftIO))
import Control.Monad.Reader       (ask, runReaderT)
import Control.Monad.Trans.Either (runEitherT, hoistEither)
import Data.ByteString.Char8      (pack)
import Data.Monoid                ((<>))
import Data.Aeson                 (parseJSON, json', FromJSON)
import Data.Aeson.Types           (parseEither)
import Network.Http.Client        (Request, Connection, Method (GET), baselineContextSSL, 
                                   buildRequest, closeConnection, concatHandler, concatHandler', 
                                   getStatusCode, http, openConnectionSSL, receiveResponse, sendRequest,
                                   setHeader, emptyBody, Response, StatusCode)
import Network                    (withSocketsDo)
import Network.HTTP.Types         (urlEncode)
import OpenSSL                    (withOpenSSL)
import System.IO.Streams          (InputStream)
import System.IO.Streams.Attoparsec (parseFromStream)

import Pinboard.Client.Types
import Pinboard.Client.Error
import Pinboard.Client.Util

import qualified Data.ByteString             as S
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T


-- | Create a default PinboardConfig using the supplied apiToken
fromApiToken :: String -> PinboardConfig
fromApiToken token = PinboardConfig { debug = False, apiToken = pack token }

--------------------------------------------------------------------------------
-- | Execute computations in the Pinboard monad
runPinboard
    :: PinboardConfig
    -> Pinboard a
    -> IO (Either PinboardError a)
runPinboard config requests = withOpenSSL $
  bracket connOpen connClose (either (connFail ConnectionFailure) go)
  where go conn = runReaderT (runEitherT requests) (config, conn) 
                  `catch` connFail UnknownErrorType

-- | Create a Pinboard value from a PinboardRequest w/ json deserialization
pinboardJson :: FromJSON a => PinboardRequest -> Pinboard a
pinboardJson req = do 
  (config, conn)  <- ask
  (_, result) <- liftIO $ sendPinboardRequest (ensureResultFormatType FormatJson req) config conn parseJSONResponseStream
  hoistEither result


--------------------------------------------------------------------------------

runPinboardSingleRaw
    :: PinboardConfig       
    -> PinboardRequest
    -> (Response -> InputStream S.ByteString -> IO a)
    -> IO (Either PinboardError a)
runPinboardSingleRaw config req handler = withOpenSSL $ 
  bracket connOpen connClose (either (connFail ConnectionFailure) go)
    where go conn = (Right <$> sendPinboardRequest req config conn handler)
                    `catch` connFail UnknownErrorType 

runPinboardSingleRawBS
    :: PinboardConfig       
    -> PinboardRequest
    -> IO (Either PinboardError S.ByteString)
runPinboardSingleRawBS config req = runPinboardSingleRaw config req concatHandler'

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
      -> Connection 
      -> (Response -> InputStream S.ByteString -> IO a)
      -> IO a
sendPinboardRequest PinboardRequest{..} PinboardConfig{..} conn handler = do
   let url = S.concat [ T.encodeUtf8 requestPath 
                      , "?" 
                      , paramsToByteString $ ("auth_token", urlEncode False apiToken) : encodeParams requestParams ]
   req <- buildReq url
   sendRequest conn req emptyBody
   receiveResponse conn handler


sendPinboardRequestBS 
  :: PinboardRequest 
  -> PinboardConfig 
  -> Connection 
  -> IO (Response, S.ByteString) 
sendPinboardRequestBS request config conn = sendPinboardRequest request config conn handler
  where handler response responseInputStream = do resultBS <- concatHandler response responseInputStream
                                                  return (response, resultBS)

--------------------------------------------------------------------------------

buildReq ::  S.ByteString -> IO Request
buildReq url = buildRequest $ do
  http GET ("/v1/" <> url)
  setHeader "Connection" "Keep-Alive"  
  setHeader "User-Agent" "pinboard.hs/0.6.1"  

--------------------------------------------------------------------------------

parseJSONResponseStream 
    :: FromJSON a 
    => Response 
    -> InputStream S.ByteString
    -> IO (Response, Either PinboardError a)
parseJSONResponseStream response stream = 
  (response,) <$> either (return . Left . addErrMsg (toText response)) 
                         (const $ parseJSONFromStream stream) 
                         (checkStatusCode $ getStatusCode response)


parseJSONFromStream 
    :: FromJSON a 
    => InputStream S.ByteString 
    -> IO (Either PinboardError a)
parseJSONFromStream s = do 
  r <- parseFromStream (parseEither parseJSON <$> json') s
  return $ either (Left . createParserErr . toText)  Right r
  `catch` connFail ParseFailure

--------------------------------------------------------------------------------

checkStatusCode :: StatusCode -> Either PinboardError ()
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


connOpenRaw :: IO Connection
connOpenRaw = withSocketsDo $ do
  ctx <- baselineContextSSL
  openConnectionSSL ctx "api.pinboard.in" 443

connOpen :: IO (Either SomeException Connection)
connOpen = try connOpenRaw

connClose :: Either a Connection -> IO ()
connClose = either (const $ return ()) closeConnection

connFail :: PinboardErrorType -> SomeException -> IO (Either PinboardError b)
connFail e msg = return $ Left $ PinboardError e (toText msg) Nothing Nothing Nothing


