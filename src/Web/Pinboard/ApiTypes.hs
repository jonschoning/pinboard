{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Web.Pinboard.Types
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Pinboard.ApiTypes where

import           Prelude hiding(words)
import           Control.Applicative        ((<$>), (<*>))
import           Data.Aeson                 (FromJSON (parseJSON), Value (String, Object), (.:))
import           Data.Text                  (Text, words, unpack)
import           Data.Time                  (UTCTime)
import Data.HashMap.Strict(toList)
import Data.Time.Format(readTime)
import System.Locale(defaultTimeLocale)
-- import Data.Map.Strict(Map)

data Posts = Posts {
      postsDate         :: UTCTime
    , postsUser         :: Text
    , posts             :: [Post]
    } deriving (Show, Eq)

instance FromJSON Posts where
   parseJSON (Object o) =
       Posts <$> o .: "date"
             <*> o .: "user"
             <*> o .: "posts"
   parseJSON _ = error "bad parse"

data Post = Post {
      postHref         :: Text
    , postDescription  :: Text
    , postExtended     :: Text
    , postMeta         :: Text
    , postHash         :: Text
    , postTime         :: UTCTime
    , postShared       :: Bool
    , postToread       :: Bool
    , postTags         :: [Text]
    } deriving (Show, Eq)

instance FromJSON Post where
   parseJSON (Object o) =
       Post <$> o .: "href"
            <*> o .: "description"
            <*> o .: "extended"
            <*> o .: "meta"
            <*> o .: "hash"
            <*> o .: "time"
            <*> (boolFromYesNo <$> o .: "shared")
            <*> (boolFromYesNo <$> o .: "toread")
            <*> (words <$> o .: "tags")
   parseJSON _ = error "bad parse"

boolFromYesNo :: Text -> Bool
boolFromYesNo "yes" = True
boolFromYesNo _     = False

data Dates = Dates {
      datesUser         :: Text
    , datesTag          :: Text
    , dates             :: [Date]
    } deriving (Show, Eq)

instance FromJSON Dates where
   parseJSON (Object o) =
       Dates <$> o .: "user"
             <*> o .: "tag"
             <*> (toDates <$> o .: "dates")
   parseJSON _ = error "bad parse"

toDates :: Value -> [Date]
toDates (Object o)= do
   (dateStr, String countStr) <- toList o
   return $ Date 
             (readTime defaultTimeLocale "%F" (unpack dateStr)) -- UTCTime
             (read (unpack countStr))                           -- Int
toDates _ = []

data Date = Date {
      dateDate         :: UTCTime
    , dateCount        :: Int
    } deriving (Show, Eq)

