{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------
-- |
-- Module      : Pinboard.Api
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https://pinboard.in/api/ >
--
-- Provides Pinboard Api Access (deserializes into Haskell data structures)

module Pinboard.Api
    ( 
      -- ** Posts
      getPostsRecent,
      getPostsForDate,
      getPostsAll,
      getPostsDates,
      getPostsMRUTime,
      getSuggestedTags,
      addPost,
      addPostRec,
      deletePost,
      -- ** Tags
      getTags,
      renameTag,
      deleteTag,
      -- ** User
      getUserSecretRssKey,
      getUserApiToken,
      -- ** Notes
      getNoteList,
      getNote,
    ) where

import Pinboard.Client          (pinboardJson)
import Pinboard.Client.Types    (Pinboard)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Pinboard.Client.Types    (ResultFormatType (..))
import Pinboard.ApiTypes        
import Pinboard.ApiRequest

import Control.Applicative
import Prelude                                            

-- POSTS ---------------------------------------------------------------------

-- | posts/recent : Returns a list of the user's most recent posts, filtered by tag.
getPostsRecent
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Maybe Count -- ^ number of results to return. Default is 15, max is 100  
  -> Pinboard Posts
getPostsRecent tags count = pinboardJson $ getPostsRecentRequest FormatJson tags count

-- | posts/all : Returns all bookmarks in the user's account.
getPostsAll
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Maybe StartOffset -- ^ offset value (default is 0)
  -> Maybe NumResults -- ^ number of results to return. Default is all
  -> Maybe FromDateTime -- ^ return only bookmarks created after this time
  -> Maybe ToDateTime -- ^ return only bookmarks created before this time
  -> Maybe Meta -- ^ include a change detection signature for each bookmark
  -> Pinboard [Post]
getPostsAll tags start results fromdt todt meta = pinboardJson $ getPostsAllRequest FormatJson tags start results fromdt todt meta 

-- | posts/get : Returns one or more posts on a single day matching the arguments. 
-- If no date or url is given, date of most recent bookmark will be used.
getPostsForDate
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Maybe Date -- ^ return results bookmarked on this day
  -> Maybe Url -- ^ return bookmark for this URL
  -> Pinboard Posts
getPostsForDate tags date url = pinboardJson $ getPostsForDateRequest FormatJson tags date url

-- | posts/dates : Returns a list of dates with the number of posts at each date.
getPostsDates
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Pinboard PostDates
getPostsDates tags = pinboardJson $ getPostsDatesRequest FormatJson tags


-- | posts/update : Returns the most recent time a bookmark was added, updated or deleted.
getPostsMRUTime :: Pinboard UTCTime
getPostsMRUTime = fromUpdateTime <$> pinboardJson (getPostsMRUTimeRequest FormatJson)

-- | posts/suggest : Returns a list of popular tags and recommended tags for a given URL. 
-- Popular tags are tags used site-wide for the url; 
-- Recommended tags are drawn from the user's own tags.
getSuggestedTags
  :: Url
  -> Pinboard [Suggested]
getSuggestedTags url = pinboardJson $ getSuggestedTagsRequest FormatJson url

-- | posts/delete : Delete an existing bookmark.
deletePost 
  :: Url
  -> Pinboard ()
deletePost url = fromDoneResult <$> (pinboardJson $ deletePostRequest FormatJson url)

-- | posts/add : Add or Update a bookmark
addPost
  :: Url            -- ^ the URL of the item
  -> Description    -- ^ Title of the item. This field is unfortunately named 'description' for backwards compatibility with the delicious API
  -> Maybe Extended -- ^ Description of the item. Called 'extended' for backwards compatibility with delicious API
  -> Maybe [Tag]    -- ^ List of up to 100 tags
  -> Maybe DateTime -- ^ creation time for this bookmark. Defaults to current time. Datestamps more than 10 minutes ahead of server time will be reset to current server time
  -> Maybe Replace  -- ^ Replace any existing bookmark with this URL. Default is yes. If set to no, will fail if bookmark exists
  -> Maybe Shared   -- ^ Make bookmark public. Default is "yes" unless user has enabled the "save all bookmarks as private" user setting, in which case default is "no"
  -> Maybe ToRead   -- ^ Marks the bookmark as unread. Default is "no"
  -> Pinboard ()
addPost url descr ext tags ctime repl shared toread = fromDoneResult <$> (pinboardJson $ addPostRequest FormatJson url descr ext tags ctime repl shared toread)

-- | posts/add :  Add or Update a bookmark, from a Post record
addPostRec
  :: Post         -- ^ a Post record
  -> Replace      -- ^ Replace any existing bookmark with the Posts URL. If set to no, will fail if bookmark exists 
  -> Pinboard ()
addPostRec post replace = fromDoneResult <$> (pinboardJson $ addPostRecRequest FormatJson post replace)

-- TAGS ----------------------------------------------------------------------


-- | tags/get : Returns a full list of the user's tags along with the number of 
-- times they were used.
getTags 
  :: Pinboard TagMap
getTags = fromJsonTagMap <$> (pinboardJson $ getTagsRequest FormatJson )


-- | tags/delete : Delete an existing tag.
deleteTag 
  :: Tag 
  -> Pinboard ()
deleteTag tag = fromDoneResult <$> (pinboardJson $ deleteTagRequest FormatJson tag)


-- | tags/rename : Rename an tag, or fold it in to an existing tag
renameTag 
  :: Old -- ^ note: match is not case sensitive
  -> New -- ^ if empty, nothing will happen
  -> Pinboard ()
renameTag old new = fromDoneResult <$> (pinboardJson $ renameTagRequest FormatJson old new)


-- USER ----------------------------------------------------------------------

-- | user/secret : Returns the user's secret RSS key (for viewing private feeds)
getUserSecretRssKey 
  :: Pinboard Text
getUserSecretRssKey = fromTextResult <$> (pinboardJson $ getUserSecretRssKeyRequest FormatJson )

-- | user/api_token : Returns the user's API token (for making API calls without a password)
getUserApiToken 
  :: Pinboard Text
getUserApiToken = fromTextResult <$> (pinboardJson $ getUserApiTokenRequest FormatJson )


-- NOTES ---------------------------------------------------------------------

-- | notes/list : Returns a list of the user's notes (note text detail is not included)
getNoteList 
  :: Pinboard NoteList
getNoteList = pinboardJson $ getNoteListRequest FormatJson 

-- | notes/id : Returns an individual user note. The hash property is a 20 character long sha1 hash of the note text.
getNote 
  :: NoteId
  -> Pinboard Note
getNote noteid = pinboardJson $ getNoteRequest FormatJson noteid

