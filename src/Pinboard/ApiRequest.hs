{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------
-- |
-- Module      : Pinboard.ApiRequest
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- These request builders allow you to build request params which can 
-- sent via Pinboard.Client.Internal, in the case you need more control
-- for how the response should be processed over what Pinboard.Api provides.

module Pinboard.ApiRequest
    ( 
      -- ** Posts
      getPostsRecentRequest,
      getPostsForDateRequest,
      getPostsAllRequest,
      getPostsDatesRequest,
      getPostsMRUTimeRequest,
      getSuggestedTagsRequest,
      addPostRequest,
      deletePostRequest,
      -- ** Tags
      getTagsRequest,
      renameTagRequest,
      deleteTagRequest,
      -- ** User
      getUserSecretRssKeyRequest,
      getUserApiTokenRequest,
      -- ** Notes
      getNoteListRequest,
      getNoteRequest,
    ) where

import Prelude hiding (unwords)
import Pinboard.Client.Types    (PinboardRequest (..), Param (..))
import Pinboard.Client.Util     ((</>))
import Control.Applicative      ((<$>))
import Data.Text                (unwords)
import Data.Maybe               (catMaybes)
import Pinboard.ApiTypes        
                                            
-- POSTS ---------------------------------------------------------------------

-- | posts/recent : Returns a list of the user's most recent posts, filtered by tag.
getPostsRecentRequest
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Maybe Count -- ^ number of results to return. Default is 15, max is 100  
  -> PinboardRequest
getPostsRecentRequest tags count = PinboardRequest path params
  where 
    path = "posts/recent" 
    params = catMaybes [ Tag . unwords <$> tags
                       , Count <$> count ]

-- | posts/all : Returns all bookmarks in the user's account.
getPostsAllRequest
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Maybe StartOffset -- ^ offset value (default is 0)
  -> Maybe NumResults -- ^ number of results to return. Default is all
  -> Maybe FromDateTime -- ^ return only bookmarks created after this time
  -> Maybe ToDateTime -- ^ return only bookmarks created before this time
  -> Maybe Meta -- ^ include a change detection signature for each bookmark
  -> PinboardRequest
getPostsAllRequest tags start results fromdt todt meta = 
  PinboardRequest path params
  where 
    path = "posts/all" 
    params = catMaybes [ Tag . unwords <$> tags
                       , Start <$> start
                       , Results <$> results
                       , FromDateTime <$> fromdt
                       , ToDateTime <$> todt
                       , Meta <$> meta
                       ]

-- | posts/get : Returns one or more posts on a single day matching the arguments. 
-- If no date or url is given, date of most recent bookmark will be used.
getPostsForDateRequest
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Maybe Date -- ^ return results bookmarked on this day
  -> Maybe Url -- ^ return bookmark for this URL
  -> PinboardRequest
getPostsForDateRequest tags date url = PinboardRequest path params
  where 
    path = "posts/get" 
    params = catMaybes [ Tag . unwords <$> tags
                       , Date <$> date
                       , Url <$> url ]


-- | posts/dates : Returns a list of dates with the number of posts at each date.
getPostsDatesRequest
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> PinboardRequest
getPostsDatesRequest tags = PinboardRequest path params
  where 
    path = "posts/dates" 
    params = catMaybes [ Tag . unwords <$> tags ]


-- | posts/update : Returns the most recent time a bookmark was added, updated or deleted.
getPostsMRUTimeRequest 
  :: PinboardRequest
getPostsMRUTimeRequest = PinboardRequest path params
  where 
    path = "posts/update" 
    params = []

-- | posts/suggest : Returns a list of popular tags and recommended tags for a given URL. 
-- Popular tags are tags used site-wide for the url; 
-- Recommended tags are drawn from the user's own tags.
getSuggestedTagsRequest
  :: Url
  -> PinboardRequest
getSuggestedTagsRequest url = PinboardRequest path params
  where 
    path = "posts/suggest" 
    params = [ Url url ]

-- | posts/delete : Delete an existing bookmark.
deletePostRequest 
  :: Url
  -> PinboardRequest
deletePostRequest url = PinboardRequest path params
  where 
    path = "posts/delete" 
    params = [Url url]


-- | posts/add : Add a bookmark
addPostRequest
  :: Url            -- ^ the URL of the item
  -> Description    -- ^ Title of the item. This field is unfortunately named 'description' for backwards compatibility with the delicious API
  -> Maybe Extended -- ^ Description of the item. Called 'extended' for backwards compatibility with delicious API
  -> Maybe [Tag]    -- ^ List of up to 100 tags
  -> Maybe DateTime -- ^ creation time for this bookmark. Defaults to current time. Datestamps more than 10 minutes ahead of server time will be reset to current server time
  -> Maybe Replace  -- ^ Replace any existing bookmark with this URL. Default is yes. If set to no, will throw an error if bookmark exists
  -> Maybe Shared   -- ^ Make bookmark public. Default is "yes" unless user has enabled the "save all bookmarks as private" user setting, in which case default is "no"
  -> Maybe ToRead   -- ^ Marks the bookmark as unread. Default is "no"
  -> PinboardRequest
addPostRequest url descr ext tags ctime repl shared toread = 
  PinboardRequest path params
  where 
    path = "posts/add" 
    params = catMaybes [ Just $ Url url
                       , Just $ Description descr
                       , Extended <$> ext
                       , Tags . unwords <$> tags
                       , DateTime <$> ctime
                       , Replace <$> repl
                       , Shared <$> shared
                       , ToRead <$> toread ]

-- TAGS ----------------------------------------------------------------------


-- | tags/get : Returns a full list of the user's tags along with the number of 
-- times they were used.
getTagsRequest
  :: PinboardRequest
getTagsRequest = PinboardRequest path params
  where 
    path = "tags/get" 
    params = []


-- | tags/delete : Delete an existing tag.
deleteTagRequest 
  :: Tag 
  -> PinboardRequest
deleteTagRequest tag = PinboardRequest path params
  where 
    path = "tags/delete" 
    params = [Tag tag]


-- | tags/rename : Rename an tag, or fold it in to an existing tag
renameTagRequest 
  :: Old -- ^ note: match is not case sensitive
  -> New -- ^ if empty, nothing will happen
  -> PinboardRequest
renameTagRequest old new = PinboardRequest path params
  where 
    path = "tags/rename" 
    params = [Old old, New new]


-- USER ----------------------------------------------------------------------

-- | user/secret : Returns the user's secret RSS key (for viewing private feeds)
getUserSecretRssKeyRequest 
  :: PinboardRequest
getUserSecretRssKeyRequest = PinboardRequest path params
  where 
    path = "user/secret" 
    params = []

-- | user/api_token : Returns the user's API token (for making API calls without a password)
getUserApiTokenRequest 
  :: PinboardRequest
getUserApiTokenRequest = PinboardRequest path params
  where 
    path = "user/api_token" 
    params = []


-- NOTES ---------------------------------------------------------------------

-- | notes/list : Returns a list of the user's notes (note text detail is not included)
getNoteListRequest 
  :: PinboardRequest
getNoteListRequest = PinboardRequest path params
  where 
    path = "notes/list" 
    params = []

-- | notes/id : Returns an individual user note. The hash property is a 20 character long sha1 hash of the note text.
getNoteRequest 
  :: NoteId
  -> PinboardRequest
getNoteRequest noteid = PinboardRequest path params
  where 
    path = "notes" </> noteid
    params = []

