{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Pinboard.ApiTypes
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Pinboard.ApiTypes where

import Data.Aeson          (FromJSON (parseJSON), Value (String, Object), ( .:))
import Data.Aeson.Types    (Parser)
import Data.HashMap.Strict (HashMap, member, toList)
import Data.Data           (Data, Typeable)
import Data.Text           (Text, words, unpack)
import Data.Time           (UTCTime)
import Data.Time.Calendar  (Day)
import Data.Time.Format    (readTime)
import System.Locale       (defaultTimeLocale)
import qualified Data.HashMap.Strict as HM

import Control.Applicative 
import Prelude hiding      (words)

-- * Posts

data Posts = Posts {
      postsDate         :: UTCTime
    , postsUser         :: Text
    , postsPosts        :: [Post]
    } deriving (Show, Eq, Data, Typeable, Ord)

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
    , postTags         :: [Tag]
    } deriving (Show, Eq, Data, Typeable, Ord)

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

data PostDates = PostDates {
      postDatesUser     :: Text
    , postDatesTag      :: Text
    , postDatesCount    :: [DateCount]
    } deriving (Show, Eq, Data, Typeable, Ord)

instance FromJSON PostDates where
   parseJSON (Object o) =
     PostDates <$> o .: "user"
               <*> o .: "tag"
               <*> (parseDates <$> o .: "dates")
     where
       parseDates :: Value -> [DateCount]
       parseDates (Object o')= do
          (dateStr, String countStr) <- toList o'
          return (read (unpack dateStr), read (unpack countStr))
       parseDates _ = []
   parseJSON _ = error "bad parse"

type DateCount = (Day, Int)


-- * Notes

data NoteList = NoteList {
      noteListCount     :: Int
    , noteListItems     :: [NoteListItem]
    } deriving (Show, Eq, Data, Typeable, Ord)

instance FromJSON NoteList where
   parseJSON (Object o) =
       NoteList <$> o .: "count"
                <*> o .: "notes"
   parseJSON _ = error "bad parse"

data NoteListItem = NoteListItem {
      noteListItemId     :: Text
    , noteListItemHash   :: Text
    , noteListItemTitle  :: Text
    , noteListItemLength :: Int
    , noteListItemCreatedAt :: UTCTime
    , noteListItemUpdatedAt :: UTCTime
    } deriving (Show, Eq, Data, Typeable, Ord)

instance FromJSON NoteListItem where
   parseJSON (Object o) =
       NoteListItem <$> o .: "id"
                    <*> o .: "hash"
                    <*> o .: "title"
                    <*> (read <$> (o .: "length"))
                    <*> (readNoteTime <$> o .: "created_at")
                    <*> (readNoteTime <$> o .: "updated_at")
   parseJSON _ = error "bad parse"



data Note = Note {
      noteId     :: Text
    , noteHash   :: Text
    , noteTitle  :: Text
    , noteText   :: Text
    , noteLength :: Int
    , noteCreatedAt :: UTCTime
    , noteUpdatedAt :: UTCTime
    } deriving (Show, Eq, Data, Typeable, Ord)

instance FromJSON Note where
   parseJSON (Object o) =
       Note <$> o .: "id"
            <*> o .: "hash"
            <*> o .: "title"
            <*> o .: "text"
            <*> o .: "length"
            <*> (readNoteTime <$> o .: "created_at")
            <*> (readNoteTime <$> o .: "updated_at")
   parseJSON _ = error "bad parse"

readNoteTime :: String -> UTCTime
readNoteTime = readTime defaultTimeLocale "%F %T"


-- * Tags

type TagMap = HashMap Tag Int

newtype JsonTagMap = ToJsonTagMap {fromJsonTagMap :: TagMap}
    deriving (Show, Eq, Data, Typeable)

instance FromJSON JsonTagMap where
  parseJSON = return . toTags
    where toTags (Object o) = ToJsonTagMap $ HM.map (\(String s)-> read (unpack s)) o
          toTags _ = error "bad parse"


data Suggested = Popular [Text]
               | Recommended [Text]
    deriving (Show, Eq, Data, Typeable, Ord)

instance FromJSON Suggested where
   parseJSON (Object o)
     | member "popular" o = Popular <$> (o .: "popular")
     | member "recommended" o = Recommended  <$> (o .: "recommended")
     | otherwise = error "bad parse"  
   parseJSON _ = error "bad parse"


-- * Scalars

newtype DoneResult = ToDoneResult {fromDoneResult :: ()}
    deriving (Show, Eq, Data, Typeable, Ord)

instance FromJSON DoneResult where
  parseJSON (Object o) = parseDone =<< (o .: "result" <|> o .: "result_code")
    where
      parseDone :: Text -> Parser DoneResult
      parseDone "done" = return $ ToDoneResult ()
      parseDone msg = ( fail . unpack ) msg
  parseJSON _ = error "bad parse"

newtype TextResult = ToTextResult {fromTextResult :: Text}
    deriving (Show, Eq, Data, Typeable, Ord)

instance FromJSON TextResult where
  parseJSON (Object o) = ToTextResult <$> (o .: "result")
  parseJSON _ = error "bad parse"

newtype UpdateTime = ToUpdateTime {fromUpdateTime :: UTCTime}
    deriving (Show, Eq, Data, Typeable, Ord)

instance FromJSON UpdateTime where
  parseJSON (Object o) = ToUpdateTime <$> (o .: "update_time")
  parseJSON _ = error "bad parse"

-- * Aliases

-- | as defined by RFC 3986. Allowed schemes are http, https, javascript, mailto, ftp and file. The Safari-specific feed scheme is allowed but will be treated as a synonym for http.
type Url = Text

-- | up to 255 characters long
type Description = Text 

-- | up to 65536 characters long. Any URLs will be auto-linkified when displayed.
type Extended = Text

-- | up to 255 characters. May not contain commas or whitespace.
type Tag = Text 
type Old = Tag
type New = Tag

type Count = Int
type NumResults = Int
type StartOffset = Int

type Shared = Bool
type Replace = Bool
type ToRead = Bool

-- | UTC date in this format: 2010-12-11. Same range as datetime above
type Date = Day

-- | UTC timestamp in this format: 2010-12-11T19:48:02Z. Valid date range is Jan 1, 1 AD to January 1, 2100 (but see note below about future timestamps).
type DateTime = UTCTime
type FromDateTime = DateTime
type ToDateTime = DateTime

type Meta = Int

type NoteId = Text

