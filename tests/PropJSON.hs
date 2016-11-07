{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module PropJSON where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Monoid ((<>))
import Data.Typeable (Proxy(..), typeOf, Typeable)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.Hspec.QuickCheck (prop)

import ApproxEq

type ArbitraryJSON a = (Arbitrary a, ToJSON a, FromJSON a, Show a, Typeable a)

propJSON
  :: forall a b.
     (ArbitraryJSON a, Testable b)
  => String -> (a -> a -> b) -> Proxy a -> Spec
propJSON eqDescr eq _ =
  prop
    (show (typeOf (undefined :: a)) <> " FromJSON/ToJSON roundtrip " <> eqDescr) $
  \(x :: a) ->
     let actual = parseEither parseJSON (toJSON x)
         expected = Right x
         failMsg =
           "ACTUAL: " <> show actual <> "\nJSON: " <> BL8.unpack (encode x)
     in counterexample failMsg $
        either reject property (eq <$> actual <*> expected)
  where
    reject = property . const rejected

propJSONEq
  :: (ArbitraryJSON a, Eq a)
  => Proxy a -> Spec
propJSONEq = propJSON "(Eq)" (==)

propJSONApproxEq
  :: (ArbitraryJSON a, ApproxEq a)
  => Proxy a -> Spec
propJSONApproxEq = propJSON "(ApproxEq)" (==~)
