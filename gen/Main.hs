{-# LANGUAGE TemplateHaskell #-}

module Main where

import LensUtil

import Pinboard
import Pinboard.ApiTypes

import Control.Lens
import Language.Haskell.TH

import Data.Aeson          
import Data.Aeson.Types    (Parser)
import Data.HashMap.Strict (HashMap, member, toList)
import Data.Data           (Data, Typeable)
import Data.Text           (Text, words, unwords, unpack, pack)
import Data.Time           (UTCTime)
import Data.Time.Calendar  (Day)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Data.Time.Format    (readTime, formatTime)
import System.Locale       (defaultTimeLocale)
import Control.Applicative 
import Prelude hiding      (words, unwords)
                                       
main = 
  putStrLn $(do 
      q0 <- makeLensesCustom ''Posts
      q1 <- makeLensesCustom ''Post
      q2 <- makeLensesCustom ''PostDates
      q3 <- makeLensesCustom ''NoteList
      q4 <- makeLensesCustom ''NoteListItem
      q5 <- makeLensesCustom ''Note
      q6 <- makePrisms ''Suggested
      let qs = concat [q0, q1, q2, q3, q4, q5 ,q6]
      stringE (pprint qs))
