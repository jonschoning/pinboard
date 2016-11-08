{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Pinboard.Util
-- Copyright   : (c) Jon Schoning, 2015
-- Maintainer  : jonschoning@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Pinboard.Util
  ( paramsToByteString
  , toText
  , toTextLower
  , (</>)
  , paramToName
  , paramToText
  , encodeParams
  , ensureResultFormatType
  ) where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (urlEncode)

import Data.Monoid

import Prelude

import Pinboard.Types

------------------------------------------------------------------------------
-- | Conversion from a `Show` constrained type to `Text`
toText
  :: Show a
  => a -> Text
toText = T.pack . show

------------------------------------------------------------------------------
-- | Conversion from a `Show` constrained type to lowercase `Text`
toTextLower
  :: Show a
  => a -> Text
toTextLower = T.toLower . T.pack . show

------------------------------------------------------------------------------
-- | Conversion of a key value pair to a query parameterized string
paramsToByteString
  :: (Monoid m, IsString m)
  => [(m, m)] -> m
paramsToByteString [] = mempty
paramsToByteString [(x, y)] = x <> "=" <> y
paramsToByteString ((x, y):xs) =
  mconcat [x, "=", y, "&"] <> paramsToByteString xs

-- | Retrieve and encode the optional parameters
encodeParams :: [Param] -> ParamsBS
encodeParams xs = do
  x <- xs
  let (k, v) = paramToText x
  return (T.encodeUtf8 k, (urlEncode True . T.encodeUtf8) v)

ensureResultFormatType :: ResultFormatType -> PinboardRequest -> PinboardRequest
ensureResultFormatType fmt req =
  if hasFormat
    then req
    else req
         { requestParams = Format fmt : params
         }
  where
    params = requestParams req
    hasFormat = Format fmt `elem` params

paramToText :: Param -> (Text, Text)
paramToText (Tag a) = ("tag", a)
paramToText (Tags a) = ("tags", a)
paramToText (Old a) = ("old", a)
paramToText (New a) = ("new", a)
paramToText (Format FormatJson) = ("format", "json")
paramToText (Format FormatXml) = ("format", "xml")
paramToText (Count a) = ("count", toText a)
paramToText (Start a) = ("start", toText a)
paramToText (Results a) = ("results", toText a)
paramToText (Url a) = ("url", a)
paramToText (Date a) = ("dt", toText a)
paramToText (DateTime a) = ("dt", toText a)
paramToText (FromDateTime a) = ("fromdt", toText a)
paramToText (ToDateTime a) = ("todt", toText a)
paramToText (Replace a) =
  ( "replace"
  , if a
      then "yes"
      else "no")
paramToText (Shared a) =
  ( "shared"
  , if a
      then "yes"
      else "no")
paramToText (ToRead a) =
  ( "toread"
  , if a
      then "yes"
      else "no")
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
  => m -> m -> m
m1 </> m2 = m1 <> "/" <> m2
