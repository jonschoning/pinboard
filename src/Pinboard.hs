-------------------------------------------
-- |
-- Module      : Pinboard
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- @
-- import Pinboard
-- 
-- main :: IO ()
-- main = do
--   let config = fromApiToken "api token"
--   result <- runPinboard config $ getPostsRecent Nothing Nothing
--   case result of
--     Right details -> print details
--     Left pinboardError -> print pinboardError
-- @

module Pinboard (
      -- * Pinboard.Client
      -- | Executes the methods defined in Pinboard.Api
      module Pinboard.Client 
      -- * Pinboard.Api
      -- | Provides Pinboard Api Access (deserializes into Haskell data structures)
    , module Pinboard.Api
      -- | Alternate Request Builders
    , module Pinboard.ApiRequest
      -- * Pinboard.ApiTypes
      -- | Pinboard Data Structures returned by the Api
    , module Pinboard.ApiTypes
  ) where

import Pinboard.Client
import Pinboard.Api
import Pinboard.ApiRequest
import Pinboard.ApiTypes
