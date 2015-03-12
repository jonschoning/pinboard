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
import           Data.Aeson                 (FromJSON (parseJSON),
                                             Value (Object), (.:))
import           Data.Text                  (Text, words)
import           Data.Time                  (UTCTime)

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

