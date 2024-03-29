{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      : Pinboard.ApiTypes
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Pinboard.ApiTypes where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Key as A
import Data.HashMap.Strict (HashMap)
import Data.Data (Data, Typeable)
import Data.Text (Text, words, unwords, unpack, pack)
import Data.Time (UTCTime, parseTimeM)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HM
#if !MIN_VERSION_aeson(1,0,0)
import qualified Data.Vector as V
#endif
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Applicative
import Prelude hiding (words, unwords)

-- * Posts
data Posts = Posts
  { postsDate :: !UTCTime
  , postsUser :: !Text
  , postsPosts :: [Post]
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Posts where
  parseJSON (Object o) = Posts <$> o .: "date" <*> o .: "user" <*> o .: "posts"
  parseJSON _ = fail "bad parse"

instance ToJSON Posts where
  toJSON Posts {..} =
    object
      [ "date" .= toJSON postsDate
      , "user" .= toJSON postsUser
      , "posts" .= toJSON postsPosts
      ]

data Post = Post
  { postHref :: !Text
  , postDescription :: !Text
  , postExtended :: !Text
  , postMeta :: !Text
  , postHash :: !Text
  , postTime :: !UTCTime
  , postShared :: !Bool
  , postToRead :: !Bool
  , postTags :: [Tag]
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Post where
  parseJSON (Object o) =
    Post <$> o .: "href" <*> o .: "description" <*> o .: "extended" <*>
    o .: "meta" <*>
    o .: "hash" <*>
    o .: "time" <*>
    (boolFromYesNo <$> o .: "shared") <*>
    (boolFromYesNo <$> o .: "toread") <*>
    (words <$> o .: "tags")
  parseJSON _ = fail "bad parse"

instance ToJSON Post where
  toJSON Post {..} =
    object
      [ "href" .= toJSON postHref
      , "description" .= toJSON postDescription
      , "extended" .= toJSON postExtended
      , "meta" .= toJSON postMeta
      , "hash" .= toJSON postHash
      , "time" .= toJSON postTime
      , "shared" .= boolToYesNo postShared
      , "toread" .= boolToYesNo postToRead
      , "tags" .= unwords postTags
      ]

boolFromYesNo :: Text -> Bool
boolFromYesNo "yes" = True
boolFromYesNo _ = False

boolToYesNo :: Bool -> Text
boolToYesNo True = "yes"
boolToYesNo _ = "no"

data PostDates = PostDates
  { postDatesUser :: !Text
  , postDatesTag :: !Text
  , postDatesCount :: [(Day, Int)]
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON PostDates where
  parseJSON (Object o) =
    PostDates <$> o .: "user" <*> o .: "tag" <*> (parseDates <$> o .: "dates")
    where
      parseDates :: Value -> [DateCount]
      parseDates (Object o') = do
        (dateStr, String countStr) <- A.toList o'
        return (read (A.toString dateStr), read (unpack countStr))
      parseDates _ = []
  parseJSON _ = fail "bad parse"

instance ToJSON PostDates where
  toJSON PostDates {..} =
    object
      [ "user" .= toJSON postDatesUser
      , "tag" .= toJSON postDatesTag
      , "dates" .= object (dateCountToPair <$> postDatesCount)
      ]
    where
      dateCountToPair (day, count) =
        ((A.fromString . show) day, String $ (pack . show) count)

type DateCount = (Day, Int)

-- * Notes
data NoteList = NoteList
  { noteListCount :: !Int
  , noteListItems :: [NoteListItem]
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON NoteList where
  parseJSON (Object o) = NoteList <$> o .: "count" <*> o .: "notes"
  parseJSON _ = fail "bad parse"

instance ToJSON NoteList where
  toJSON NoteList {..} =
    object ["count" .= toJSON noteListCount, "notes" .= toJSON noteListItems]

data NoteListItem = NoteListItem
  { noteListItemId :: !Text
  , noteListItemHash :: !Text
  , noteListItemTitle :: !Text
  , noteListItemLength :: !Int
  , noteListItemCreatedAt :: !UTCTime
  , noteListItemUpdatedAt :: !UTCTime
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON NoteListItem where
  parseJSON (Object o) =
    NoteListItem <$> o .: "id" <*> o .: "hash" <*> o .: "title" <*>
    (read <$> (o .: "length")) <*>
    (readNoteTime =<< o .: "created_at") <*>
    (readNoteTime =<< o .: "updated_at")
  parseJSON _ = fail "bad parse"

instance ToJSON NoteListItem where
  toJSON NoteListItem {..} =
    object
      [ "id" .= toJSON noteListItemId
      , "hash" .= toJSON noteListItemHash
      , "title" .= toJSON noteListItemTitle
      , "length" .= toJSON (show noteListItemLength)
      , "created_at" .= toJSON (showNoteTime noteListItemCreatedAt)
      , "updated_at" .= toJSON (showNoteTime noteListItemUpdatedAt)
      ]

data Note = Note
  { noteId :: !Text
  , noteHash :: !Text
  , noteTitle :: !Text
  , noteText :: !Text
  , noteLength :: !Int
  , noteCreatedAt :: !UTCTime
  , noteUpdatedAt :: !UTCTime
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Note where
  parseJSON (Object o) =
    Note <$> o .: "id" <*> o .: "hash" <*> o .: "title" <*> o .: "text" <*>
    o .: "length" <*>
    (readNoteTime =<< o .: "created_at") <*>
    (readNoteTime =<< o .: "updated_at")
  parseJSON _ = fail "bad parse"

instance ToJSON Note where
  toJSON Note {..} =
    object
      [ "id" .= toJSON noteId
      , "hash" .= toJSON noteHash
      , "title" .= toJSON noteTitle
      , "text" .= toJSON noteText
      , "length" .= toJSON noteLength
      , "created_at" .= toJSON (showNoteTime noteCreatedAt)
      , "updated_at" .= toJSON (showNoteTime noteUpdatedAt)
      ]

readNoteTime
  :: MonadFail m
  => String -> m UTCTime
readNoteTime = parseTimeM True defaultTimeLocale "%F %T"

showNoteTime :: UTCTime -> String
showNoteTime = formatTime defaultTimeLocale "%F %T"

-- * Tags
type TagMap = HashMap Tag Int

newtype JsonTagMap = ToJsonTagMap
  { fromJsonTagMap :: TagMap
  } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON JsonTagMap where
  parseJSON = toTags
    where
      toTags (Object o) =
        return . ToJsonTagMap $ HM.map (\(String s) -> read (unpack s)) $ A.toHashMapText o
      toTags _ = fail "bad parse"

instance ToJSON JsonTagMap where
  toJSON (ToJsonTagMap o) = toJSON $ show <$> o

data Suggested
  = Popular [Text]
  | Recommended [Text]
  deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Suggested where
  parseJSON (Object o)
    | A.member "popular" o = Popular <$> (o .: "popular")
    | A.member "recommended" o = Recommended <$> (o .: "recommended")
    | otherwise = fail "bad parse"
  parseJSON _ = fail "bad parse"

#if !MIN_VERSION_aeson(1,0,0)
instance ToJSON [Suggested] where
  toJSON xs = Array $ toJSON <$> V.fromList xs
#endif

instance ToJSON Suggested where
  toJSON (Popular tags) = object ["popular" .= toJSON tags]
  toJSON (Recommended tags) = object ["recommended" .= toJSON tags]

-- * Scalars
newtype DoneResult = ToDoneResult
  { fromDoneResult :: ()
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON DoneResult where
  parseJSON (Object o) = parseDone =<< (o .: "result" <|> o .: "result_code")
    where
      parseDone :: Text -> Parser DoneResult
      parseDone "done" = return $ ToDoneResult ()
      parseDone msg = (fail . unpack) msg
  parseJSON _ = fail "bad parse"

newtype TextResult = ToTextResult
  { fromTextResult :: Text
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON TextResult where
  parseJSON (Object o) = ToTextResult <$> (o .: "result")
  parseJSON _ = fail "bad parse"

newtype UpdateTime = ToUpdateTime
  { fromUpdateTime :: UTCTime
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON UpdateTime where
  parseJSON (Object o) = ToUpdateTime <$> (o .: "update_time")
  parseJSON _ = error "bad parse"

-- prettyString :: String -> String
-- prettyString s = case parseExp s of
--     ParseOk x -> prettyPrint x
--     ParseFailed{} -> s
-- pretty :: Show a => a -> String
-- pretty = prettyString . show
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
