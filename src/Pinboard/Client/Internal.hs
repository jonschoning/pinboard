{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Pinboard.Client.Internal
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
------------------------------------------------------------------------------

module Pinboard.Client.Internal
    ( 
      -- * Monadic
      pinboardJson
    , runPinboardJson
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
    ) where


import           Control.Applicative        ((<$>))
import           Control.Exception          (catch, SomeException, try, bracket)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader       (ask, runReaderT)
import           Control.Monad.Trans.Either (left, runEitherT, right)
import           Data.Aeson                 (FromJSON, Value(..), eitherDecodeStrict)
import           Data.Monoid                ((<>))
import qualified Data.ByteString             as S
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Network.Http.Client        (Connection, Method (GET),
                                             baselineContextSSL, buildRequest,
                                             closeConnection, concatHandler, concatHandler', 
                                             getStatusCode, http,
                                             openConnectionSSL,
                                             receiveResponse, sendRequest,
                                             setHeader, emptyBody, Response)
import Network.HTTP.Types(urlEncode)
import           OpenSSL                    (withOpenSSL)
import           System.IO.Streams          (InputStream)
import           Pinboard.Client.Error  (PinboardError (..),
                                             PinboardErrorHTTPCode (..),
                                             PinboardErrorType (..),
                                             defaultPinboardError)
import           Pinboard.Client.Types  (Pinboard,
                                             PinboardConfig (..),
                                             PinboardRequest (..),
                                             Param (..))
import           Pinboard.Client.Util    (encodeParams, paramsToByteString, toText)

--------------------------------------------------------------------------------


pinboardJson :: FromJSON a => PinboardRequest -> Pinboard a
pinboardJson req = do 
  (config, conn)  <- ask
  result <- liftIO (sendPinboardRequestBS reqJson config conn)
  handleResultBS (debug config) result
  where
    reqJson =  req { requestParams = Format "json" : requestParams req }
    handleDecodeError dbg resultBS msg = do
      when dbg $ liftIO $ print (eitherDecodeStrict resultBS :: Either String Value)
      left $ PinboardError ParseFailure (T.pack msg) Nothing Nothing Nothing 
    handleResultBS dbg (response, resultBS) =
          case getStatusCode response of
            200 -> either (handleDecodeError dbg resultBS) right (eitherDecodeStrict resultBS)
            code | code >= 400 ->
                     let pinboardError err = left $ defaultPinboardError { errorMsg = toText resultBS, errorHTTP = Just err } in
                     case code of
                      400 -> pinboardError BadRequest
                      401 -> pinboardError UnAuthorized
                      402 -> pinboardError RequestFailed
                      403 -> pinboardError Forbidden
                      404 -> pinboardError NotFound
                      429 -> pinboardError TooManyRequests
                      500 -> pinboardError PinboardServerError
                      502 -> pinboardError PinboardServerError
                      503 -> pinboardError PinboardServerError
                      504 -> pinboardError PinboardServerError
                      _   -> pinboardError UnknownHTTPCode
            _ -> left defaultPinboardError


runPinboardJson
    :: FromJSON a
    => PinboardConfig
    -> Pinboard a
    -> IO (Either PinboardError a)
runPinboardJson config requests = withOpenSSL $
  bracket connOpen connClose (either (connFail ConnectionFailure) go)
  where go conn = runReaderT (runEitherT requests) (config, conn) 
                  `catch` connFail UnknownErrorType


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
runPinboardSingleJson config = runPinboardJson config . pinboardJson


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
  where
    buildReq url = buildRequest $ do
      http GET ("/v1/" <> url)
      setHeader "Connection" "Keep-Alive"  
      setHeader "User-Agent" "pinboard.hs/0.2"  

sendPinboardRequestBS 
  :: PinboardRequest 
  -> PinboardConfig 
  -> Connection 
  -> IO (Response, S.ByteString) 
sendPinboardRequestBS request config conn = sendPinboardRequest request config conn handler
  where handler response responseInputStream = do resultBS <- concatHandler response responseInputStream
                                                  return (response, resultBS)
                                                --
--------------------------------------------------------------------------------
connOpenRaw :: IO Connection
connOpenRaw = do
  ctx <- baselineContextSSL
  openConnectionSSL ctx "api.pinboard.in" 443

connOpen :: IO (Either SomeException Connection)
connOpen = try connOpenRaw

connClose :: Either a Connection -> IO ()
connClose = either (const $ return ()) closeConnection

connFail :: PinboardErrorType -> SomeException -> IO (Either PinboardError b)
connFail e msg = return $ Left $ PinboardError e (toText msg) Nothing Nothing Nothing


