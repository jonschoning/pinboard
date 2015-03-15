{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------
-- |
-- Module      : Web.Pinboard.Api
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https://pinboard.in/api/ >

module Web.Pinboard.Api
    ( 
      Tag,
      Old,
      New,
      Url,
      Count,
      getPosts,
      getPostsRecent,
      getPostsDates,
      getPostsUpdate,
      deletePost,
      getSuggested,
      getTags,
      deleteTag,
      renameTag
    ) where

import Prelude hiding (unwords)
import Web.Pinboard.Client.Internal (pinboardJson)
import Web.Pinboard.Client.Types    (Pinboard, PinboardRequest (..), Param (..))
import Control.Applicative          ((<$>))
import Data.Text                    (Text, unwords)
import Data.Time                    (UTCTime)
import Data.Maybe                   (catMaybes)
import Web.Pinboard.ApiTypes        
import Data.Time.Calendar(Day)
                                            
------------------------------------------------------------------------------
                                            

-- | up to 255 characters. May not contain commas or whitespace.
type Tag = Text 
type Old = Tag
type New = Tag

-- | as defined by RFC 3986. Allowed schemes are http, https, javascript, mailto, ftp and file. The Safari-specific feed scheme is allowed but will be treated as a synonym for http.
type Url = Text


type Count = Int

------------------------------------------------------------------------------

-- | Returns a list of the user's most recent posts, filtered by tag.
getPostsRecent 
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Maybe Count -- ^ number of results to return. Default is 15, max is 100  
  -> Pinboard Posts
getPostsRecent tags count = pinboardJson (PinboardRequest path params)
  where 
    path = "posts/recent" 
    params = catMaybes [ Tag . unwords <$> tags
                       , Count <$> count ]
                       --
-- | Returns one or more posts on a single day matching the arguments. 
-- If no date or url is given, date of most recent bookmark will be used.
getPosts
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Maybe Day -- ^ return results bookmarked on this day
  -> Maybe Url -- ^ return bookmark for this URL
  -> Pinboard Posts
getPosts tags date url = pinboardJson (PinboardRequest path params)
  where 
    path = "posts/get" 
    params = catMaybes [ Tag . unwords <$> tags
                       , Date <$> date
                       , Url <$> url ]

-- | Returns a list of dates with the number of posts at each date.
getPostsDates
  :: Maybe [Tag] -- ^ filter by up to three tags
  -> Pinboard PostDates
getPostsDates tags = pinboardJson (PinboardRequest path params)
  where 
    path = "posts/dates" 
    params = catMaybes [ Tag . unwords <$> tags ]

-- | Returns the most recent time a bookmark was added, updated or deleted.
-- Use this before calling posts/all to see if the data has changed since the last fetch.
getPostsUpdate :: Pinboard UTCTime
getPostsUpdate = fromUpdateTime <$> pinboardJson (PinboardRequest path params)
  where 
    path = "posts/update" 
    params = []

-- | Delete an existing bookmark.
deletePost 
  :: Url
  -> Pinboard ()
deletePost url = fromDoneResult <$> pinboardJson (PinboardRequest path params)
  where 
    path = "posts/delete" 
    params = [Url url]

-- | Delete an existing bookmark.
------------------------------------------------------------------------------

-- | Returns a list of popular tags and recommended tags for a given URL. 
-- Popular tags are tags used site-wide for the url; 
-- Recommended tags are drawn from the user's own tags.
getSuggested
  :: Url
  -> Pinboard [Suggested]
getSuggested url = pinboardJson (PinboardRequest path params)
  where 
    path = "posts/suggest" 
    params = [ Url url ]

-- | Returns a full list of the user's tags along with the number of 
-- times they were used.
getTags :: Pinboard TagMap
getTags = fromJsonTagMap <$> pinboardJson (PinboardRequest path params)
  where 
    path = "tags/get" 
    params = []

-- | Delete an existing tag.
deleteTag 
  :: Tag 
  -> Pinboard ()
deleteTag tag = fromDoneResult <$> pinboardJson (PinboardRequest path params)
  where 
    path = "tags/delete" 
    params = [Tag tag]


-- | Rename an tag, or fold it in to an existing tag
renameTag 
  :: Old -- ^ note: match is not case sensitive
  -> New -- ^ if empty, nothing will happen
  -> Pinboard ()
renameTag old new = fromDoneResult <$> pinboardJson (PinboardRequest path params)
  where 
    path = "tags/rename" 
    params = [Old old, New new]

