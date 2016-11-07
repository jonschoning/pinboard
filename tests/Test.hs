{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Time.Clock (UTCTime(..))
import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import PropJSON
import Instances ()

import Pinboard

main :: IO ()
main =
  hspec $
  do prop "UTCTime" $
       \(x :: UTCTime) ->
          (readNoteTime . showNoteTime) x == (return x :: Maybe UTCTime)
     describe "JSON instances" $
       do propJSONEq (Proxy :: Proxy UTCTime)
          propJSONEq (Proxy :: Proxy Post)
          propJSONEq (Proxy :: Proxy Posts)
          propJSONEq (Proxy :: Proxy Note)
          propJSONEq (Proxy :: Proxy NoteList)
          propJSONEq (Proxy :: Proxy NoteListItem)
          propJSONEq (Proxy :: Proxy JsonTagMap)
          propJSONEq (Proxy :: Proxy Suggested)
          propJSONApproxEq (Proxy :: Proxy PostDates)
          describe "decodeJSONResponse: handle parse failures" $
            do it "malformed object parses as ParseFailure" $
                 let noteJson = "FAIL"
                 in case decodeJSONResponse noteJson of
                      Left PinboardError {..} -> errorType == ParseFailure
                      Right Note {..} -> False
               it "malformed field parses as ParseFailure" $
                 let noteJson =
                       "{\"length\":0,\"hash\":\"\",\"text_FAIL\":\"\",\"updated_at\":\"1864-05-09 13:50:53\",\"created_at\":\"1864-05-09 18:21:35\",\"id\":\"\",\"title\":\"\"}"
                 in case decodeJSONResponse noteJson of
                      Left PinboardError {..} -> errorType == ParseFailure
                      Right Note {..} -> False
               it "malformed value parses as ParseFailure" $
                 let noteJson =
                       "{\"length\":FAIL,\"hash\":\"\",\"text\":\"\",\"updated_at\":\"1864-05-09 13:50:53\",\"created_at\":\"1864-05-09 18:21:35\",\"id\":\"\",\"title\":\"\"}"
                 in case decodeJSONResponse noteJson of
                      Left PinboardError {..} -> errorType == ParseFailure
                      Right Note {..} -> False
               it "malformed time parses as ParseFailure" $
                 let noteJson =
                       "{\"length\":0,\"hash\":\"\",\"text\":\"\",\"updated_at\":\"FAIL-05-09 13:50:53\",\"created_at\":\"1864-05-09 18:21:35\",\"id\":\"\",\"title\":\"\"}"
                 in case decodeJSONResponse noteJson of
                      Left PinboardError {..} -> errorType == ParseFailure
                      Right Note {..} -> False
