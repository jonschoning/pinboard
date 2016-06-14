{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Types (parseEither)
import           Data.Char (isSpace)
import           Data.Monoid
import           Data.Text (Text, pack)
import           Data.List
import           Data.Ord
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import           Data.Typeable
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import ApproxEq

import           Pinboard

propJSON :: forall a b. (Arbitrary a, ToJSON a, FromJSON a, Show a, Typeable a, Testable b) 
         => (Either String a -> Either String a -> b) 
         -> Proxy a 
         -> Spec
propJSON eq _ = prop (show (typeOf (undefined :: a)) <> " FromJSON/ToJSON roundtrip") $ \(x :: a) ->
  let actual = parseEither parseJSON (toJSON x)
      expected = Right x
      failMsg = "ACTUAL: " <> show actual <> "\nJSON: " <> BL8.unpack (encode x)
  in counterexample failMsg (actual `eq` expected)

propJSONEq :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Show a, Typeable a, Eq a) => Proxy a -> Spec
propJSONEq = propJSON (==)

propJSONApproxEq :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Show a, Typeable a, ApproxEq a) => Proxy a -> Spec
propJSONApproxEq = propJSON (==~)

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (ModifiedJulianDay <$>) . shrink . toModifiedJulianDay

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> (secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary Note where
  arbitrary = Note <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary NoteList where
  arbitrary = NoteList <$> arbitrary <*> resize 15 arbitrary

instance Arbitrary NoteListItem where
  arbitrary = NoteListItem <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance Arbitrary Posts where
  arbitrary = Posts <$> arbitrary <*> arbitrary <*> (resize 15 arbitrary)

instance Arbitrary Post where
  arbitrary = Post <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitraryTags

instance Arbitrary JsonTagMap where
  arbitrary = ToJsonTagMap <$> (HM.fromList <$> (listOf $ (,) <$> arbitraryTag <*> arbitrary))

arbitraryTags :: Gen [Tag]
arbitraryTags = listOf arbitraryTag

arbitraryTag :: Gen Tag
arbitraryTag = pack <$> listOf1 (arbitrary `suchThat` (\c -> (not . isSpace) c && (',' /= c)))

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups :: (Ord a) => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s,
        Set.size s' > Set.size s
      = go s' xs
      | otherwise = False

instance Arbitrary PostDates where
  arbitrary = PostDates <$> arbitrary <*> arbitrary <*> (arbitrary `suchThat` isValidDateCount)
    where
      isValidDateCount xs = hasNoDups (fst <$> xs) && all (> 0) (snd <$> xs)

instance ApproxEq PostDates where
  (=~) a b =
    postDatesUser a == postDatesUser b
    && postDatesTag a == postDatesTag b
    && sorted (postDatesCount a) == sorted (postDatesCount b)
    where sorted = sortBy (comparing fst <> comparing snd)

instance Arbitrary Suggested where
  arbitrary = arbitrary >>= \a -> elements [Popular a, Recommended a]

main :: IO ()
main = hspec $ do
  prop "UTCTime" $ \(x :: UTCTime) -> (readNoteTime . showNoteTime) x == x
  describe "JSON instances" $ do
    propJSONEq (Proxy :: Proxy UTCTime)
    propJSONEq (Proxy :: Proxy Post)
    propJSONEq (Proxy :: Proxy Posts)
    propJSONEq (Proxy :: Proxy Note)
    propJSONEq (Proxy :: Proxy NoteList)
    propJSONEq (Proxy :: Proxy NoteListItem)
    propJSONEq (Proxy :: Proxy JsonTagMap)
    propJSONEq (Proxy :: Proxy Suggested)
    propJSONApproxEq (Proxy :: Proxy PostDates)
