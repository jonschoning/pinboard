module Instances where

import           Data.Text (Text, pack)
import           Data.Char (isSpace)
import           Data.List (sort)
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import           Test.QuickCheck
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import ApproxEq
import Pinboard

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
  arbitrary = Posts <$> arbitrary <*> arbitrary <*> resize 15 arbitrary

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
  arbitrary = ToJsonTagMap <$> (HM.fromList <$> listOf ((,) <$> arbitraryTag <*> arbitrary))

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

instance ApproxEq Day where (=~) = (==)
instance ApproxEq PostDates where
  (=~) a b =
    postDatesUser a =~ postDatesUser b
    && postDatesTag a =~ postDatesTag b
    && sort (postDatesCount a) =~ sort (postDatesCount b)

instance Arbitrary Suggested where
  arbitrary = arbitrary >>= \a -> elements [Popular a, Recommended a]