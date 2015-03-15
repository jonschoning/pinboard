{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Web.Pinboard.Types
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Pinboard.ApiTypes where

import Prelude hiding                      (words)
import Control.Applicative                 ((<$>), (<*>))
import Data.Aeson                          (FromJSON (parseJSON), Value (String, Object), ( .:))
import Data.HashMap.Strict                 (HashMap, member, toList)
import Data.Text                           (Text, words, unpack)
import Data.Time                           (UTCTime)
import Data.Time.Format                    (readTime)
import System.Locale                       (defaultTimeLocale)
import qualified Data.HashMap.Strict as HM
import Control.Applicative((<|>))
import Data.Aeson.Types(Parser)

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
             <*> (parseDates <$> o .: "dates")
               where
                parseDates :: Value -> [Date]
                parseDates (Object o')= do
                   (dateStr, String countStr) <- toList o'
                   return $ Date 
                             (readTime defaultTimeLocale "%F" (unpack dateStr)) -- UTCTime
                             (read (unpack countStr))                           -- Int
                parseDates _ = []
   parseJSON _ = error "bad parse"


data Date = Date {
      dateDate         :: UTCTime
    , dateCount        :: Int
    } deriving (Show, Eq)


data Suggested = Popular [Text]
               | Recommended [Text]
    deriving (Show, Eq)

instance FromJSON Suggested where
   parseJSON (Object o)
     | member "popular" o = Popular <$> (o .: "popular")
     | member "recommended" o = Recommended  <$> (o .: "recommended")
     | otherwise = error "bad parse"  
   parseJSON _ = error "bad parse"

type TagMap = HashMap Text Int

newtype JsonTagMap = ToJsonTagMap {fromJsonTagMap :: TagMap}
    deriving (Show, Eq)

instance FromJSON JsonTagMap where
  parseJSON = return . toTags
    where toTags (Object o) = ToJsonTagMap $ HM.map (\(String s)-> read (unpack s)) o
          toTags _ = error "bad parse"

newtype DoneResult = ToDoneResult {fromDoneResult :: ()}
    deriving (Show, Eq)

instance FromJSON DoneResult where
  parseJSON (Object o) = parseDone =<< (o .: "result" <|> o .: "result_code")
    where
      parseDone :: Text -> Parser DoneResult
      parseDone "done" = return $ ToDoneResult ()
      parseDone msg = ( fail . unpack ) msg
  parseJSON _ = error "bad parse"

newtype UpdateTime = ToUpdateTime {fromUpdateTime :: UTCTime}
    deriving (Show, Eq)

instance FromJSON UpdateTime where
  parseJSON (Object o) = ToUpdateTime <$> (o .: "update_time")
  parseJSON _ = error "bad parse"

