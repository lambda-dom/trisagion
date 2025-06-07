{-# LANGUAGE OverloadedStrings #-}

module Tests.Typeclasses.Streamable (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Hedgehog (Gen, Property, Group (..), (===), property, checkParallel, forAll)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))

-- Testing helpers.
import Lib.Generators (genCounter, genOffset)


-- Properties.
prop_uncons :: (Streamable s, Eq (ElementOf s), Show s, Show (ElementOf s)) => Gen s -> Property
prop_uncons gen = property $ do
    xs <- forAll gen
    toList xs === (maybe [] (\ (y, ys) -> y : toList ys) . uncons) xs


-- Main test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Typeclasses.Streamable" [
    ("prop_uncons_counter", prop_uncons genCounter),
    ("prop_uncons_offset", prop_uncons genOffset)
    ]
