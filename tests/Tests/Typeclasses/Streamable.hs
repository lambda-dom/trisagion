{-# LANGUAGE OverloadedStrings #-}

module Tests.Typeclasses.Streamable (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Testing.
import Hedgehog (Gen, Property, Group (..), (===), property, checkParallel, forAll)
import qualified Hedgehog.Range as Range (linearBounded)
import qualified Hedgehog.Gen as Gen (word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import qualified Trisagion.Streams.Counter as Counter (initialize)
import qualified Trisagion.Streams.Offset as Offset (initialize)

-- Testing helpers.
import Lib.Generators (genStream)
import Lib.FunctionExp (genFunctionExp, makeFunction)


-- Properties.
prop_uncons :: (Streamable s, Eq (ElementOf s), Show s, Show (ElementOf s)) => Gen s -> Property
prop_uncons gen = property $ do
    xs <- forAll gen
    toList xs === (maybe [] (\ (y, ys) -> y : toList ys) . uncons) xs

prop_monofunctor
    :: (Streamable s, Show s, Eq s, Show (ElementOf s), Eq (ElementOf s))
    => (ElementOf s -> ElementOf s -> ElementOf s)
    -> Gen (ElementOf s)
    -> Gen s
    -> Property
prop_monofunctor h genF genS = property $ do
    fs <- forAll (genFunctionExp genF)
    xs <- forAll genS
    (uncons . monomap (makeFunction h fs)) xs
        === (fmap (bimap (makeFunction h fs) (monomap (makeFunction h fs))) . uncons) xs


-- Main test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Typeclasses.Streamable" [
    ("prop_uncons_counter", prop_uncons (genStream Counter.initialize)),
    ("prop_uncons_offset", prop_uncons (genStream Offset.initialize)),
    ("prop_monofunctor_counter", prop_monofunctor max (Gen.word8 Range.linearBounded) (genStream Counter.initialize)),
    ("prop_monofunctor_offset", prop_monofunctor max (Gen.word8 Range.linearBounded) (genStream Offset.initialize))
    ]
