{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Typeclasses.Splittable (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Word (Word8)

-- Testing.
import Hedgehog (Property, Gen, Group (..), (===), property, forAll, checkParallel)
import qualified Hedgehog.Gen as Gen (word8, word)
import qualified Hedgehog.Range as Range (linearBounded, linear)

-- Libraries.
import Data.ByteString (ByteString)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Typeclasses.Streamable (Streamable(..))
import Trisagion.Streams.Counter (Counter)
import Trisagion.Streams.Offset (Offset)

-- Testing helpers.
import Lib.Generators (genCounter, genOffset)


-- Properties.
prop_single
    :: forall s . (Splittable s, ElementOf s ~ Word8, ElementOf (PrefixOf s) ~ Word8, MonoFoldable (PrefixOf s))
    => Property
prop_single = property $ do
    xs <- forAll $ Gen.word8 Range.linearBounded
    [xs] === (monotoList . single @s) xs

prop_splitPrefix
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ ElementOf (PrefixOf s), Show s, Eq (ElementOf s), Show (ElementOf s))
    => Gen s -> Property
prop_splitPrefix gen = property $ do
    xs <- forAll gen
    -- match size of input streams.
    size <- forAll $ Gen.word (Range.linear 0 11)
    (bimap monotoList toList . splitPrefix size) xs === (splitPrefix size . toList) xs

prop_compatibility
    :: (Splittable s, Show s, MonoFoldable (PrefixOf s), ElementOf s ~ ElementOf (PrefixOf s), Eq (ElementOf s), Show (ElementOf (PrefixOf s)))
    => Gen s -> Property
prop_compatibility gen = property $ do
    xs <- forAll gen
    (maybe ([], []) (bimap (: []) toList) . uncons) xs === (bimap monotoList toList . splitPrefix 1) xs


-- Main test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Typeclasses.Splittable" [
    ("prop_single_counter", prop_single @(Counter ByteString)),
    ("prop_single_offset", prop_single @(Offset ByteString)),
    ("prop_splitPrefix_counter", prop_splitPrefix genCounter),
    ("prop_splitPrefix_offset", prop_splitPrefix genOffset),
    ("prop_compatibility_counter", prop_compatibility genCounter),
    ("prop_compatibility_offset", prop_compatibility genOffset)
    ]
