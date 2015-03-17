-- |
-- Module      : Pinboard.Client
-- Copyright   : (c) Jon Schoning
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
--   result <- runPinboardJson config $ getPostsRecent Nothing Nothing
--   case result of
--     Right details -> print details
--     Left pinboardError -> print pinboardError
-- @
module Pinboard.Client
    ( 
      -- * Client
      -- | Executes the methods defined in Pinboard.Api
      runPinboardJson
      -- | Create a default PinboardConfig using the supplied apiToken
    , fromApiToken
      -- | The PinboardConfig provides authentication via apiToken
    , PinboardConfig       (..)
      -- * Client Dependencies
    , module Pinboard.Client.Error
    , module Pinboard.Client.Types
    , module Pinboard.Client.Util
    ) where

import Pinboard.Client.Internal
import Pinboard.Client.Types
import Pinboard.Client.Error
import Pinboard.Client.Util
import Data.ByteString.Char8(pack)

fromApiToken :: String -> PinboardConfig
fromApiToken token = PinboardConfig { debug = False, apiToken = pack token }
