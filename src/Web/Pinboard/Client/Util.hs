{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Pinboard.Client.Util
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Pinboard.Client.Util
    ( 
      mkConfig
    , fromApiToken
    , paramsToByteString
    , toText
    , toTextLower
    , (</>)
    , paramToName
    , paramToText
    , encodeParams
    ) where

import           Data.Monoid           (Monoid, mconcat, mempty, (<>))
import           Data.String           (IsString)
import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Web.Pinboard.Client.Types (PinboardConfig (..), Param (..), ParamsBS)
import Network.HTTP.Types(urlEncode)

------------------------------------------------------------------------------

mkConfig :: PinboardConfig
mkConfig = PinboardConfig { debug = False, apiToken = mempty }

fromApiToken :: ByteString -> PinboardConfig
fromApiToken token = PinboardConfig { debug = False, apiToken = token }

------------------------------------------------------------------------------
-- | Conversion from a `Show` constrained type to `Text`
toText
    :: Show a
    => a    
    -> Text 
toText = T.pack . show

------------------------------------------------------------------------------
-- | Conversion from a `Show` constrained type to lowercase `Text`
toTextLower
    :: Show a
    => a    
    -> Text 
toTextLower = T.toLower . T.pack . show

------------------------------------------------------------------------------
-- | Conversion of a key value pair to a query parameterized string
paramsToByteString
    :: (Monoid m, IsString m)
    => [(m, m)]
    -> m
paramsToByteString []           = mempty
paramsToByteString [(x,y)] = x <> "=" <> y
paramsToByteString ((x,y) : xs) =
    mconcat [ x, "=", y, "&" ] <> paramsToByteString xs

-- | Retrieve and encode the optional parameters
encodeParams :: [Param] -> ParamsBS
encodeParams xs = do 
  x <- xs 
  let (k, v) = paramToText x
  return ( T.encodeUtf8 k
         , (urlEncode True . T.encodeUtf8 ) v
         )

paramToText :: Param -> (Text, Text)
paramToText (Tag a)      = ("tag", a)
paramToText (Tags a)     = ("tags", a)
paramToText (Old a)      = ("old", a)
paramToText (New a)      = ("new", a)
paramToText (Format a)   = ("format", a)
paramToText (Count a)    = ("count", toText a)
paramToText (Start a)    = ("start", toText a)
paramToText (Results a)  = ("results", toText a)
paramToText (Url a)      = ("url", a)
paramToText (Date a)     = ("dt", toText a)
paramToText (DateTime a) = ("dt", toText a)
paramToText (FromDateTime a) = ("fromdt", toText a)
paramToText (ToDateTime a)   = ("todt", toText a)
paramToText (Replace a)  = ("replace", if a then "yes" else "no")
paramToText (Shared a)   = ("shared", if a then "yes" else "no")
paramToText (ToRead a)   = ("toread", if a then "yes" else "no")
paramToText (Description a) = ("description", a)
paramToText (Extended a) = ("extended", a)
paramToText (Meta a) = ("meta", toText a)

paramToName :: Param -> Text
paramToName = fst . paramToText
------------------------------------------------------------------------------
-- | Forward slash interspersion on `Monoid` and `IsString`
-- constrained types
(</>)
    :: (Monoid m, IsString m)
    => m
    -> m
    -> m
m1 </> m2 = m1 <> "/" <> m2
