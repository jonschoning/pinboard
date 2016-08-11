{-# LANGUAGE TemplateHaskell #-}

module Main where

import           LensUtil

import           Pinboard
import           Pinboard.ApiTypes

import           Control.Lens
import           Language.Haskell.TH

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.HashMap.Strict (HashMap, member, toList)
import           Data.Data (Data, Typeable)
import           Data.Text (Text, words, unwords, unpack, pack)
import           Data.Time (UTCTime)
import           Data.Time.Calendar (Day)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import           Control.Applicative
import           Prelude hiding (words, unwords)

main =
  putStrLn
    $(do
        let lensNames = [''Posts, ''Post, ''PostDates, ''NoteList, ''NoteListItem, ''Note]
            prismNames = [''Suggested]
            lenses = [makeLensesCustom name | name <- lensNames]
            prisms = [makePrisms name | name <- prismNames]
        decs <- sequence (lenses ++ prisms)
        (stringE . pprint) decs)
