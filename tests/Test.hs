{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Time.Clock (UTCTime(..))
import           Data.Typeable (Proxy(..))
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

import PropJSON
import Instances()

import           Pinboard

main :: IO ()
main = hspec $ do
  prop "UTCTime" $ \(x :: UTCTime) -> (readNoteTime . showNoteTime) x == x
  describe "JSON instances" $ do
    propJSONEq (Proxy :: Proxy UTCTime)
    propJSONEq (Proxy :: Proxy Post)
    propJSONEq (Proxy :: Proxy Posts)
    propJSONEq (Proxy :: Proxy Note)
    propJSONEq (Proxy :: Proxy NoteList)
    propJSONEq (Proxy :: Proxy NoteListItem)
    propJSONEq (Proxy :: Proxy JsonTagMap)
    propJSONEq (Proxy :: Proxy Suggested)
    propJSONApproxEq (Proxy :: Proxy PostDates)
