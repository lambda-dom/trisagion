{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Types.ParseError (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
-- Testing.
import Hedgehog (Property, Gen, Group (..), (===), property, forAll, checkParallel)
import qualified Hedgehog.Range as Range (linearBounded)
import qualified Hedgehog.Gen as Gen (word8)

-- Package.

-- Package testing helpers.
import Lib.Generators (genParseError)
import Lib.FunctionExp (genFunctionExp, makeFunction)
import Trisagion.Types.ParseError (ParseError)


-- Properties.
prop_leftIdentity :: (Eq e, Show e) => Gen e -> Property
prop_leftIdentity gen = property $ do
    e <- forAll (genParseError gen)
    mempty <> e === e

prop_rightIdentity :: (Eq e, Show e) => Gen e -> Property
prop_rightIdentity gen = property $ do
    e <- forAll (genParseError gen)
    e <> mempty === e

prop_Associativity :: (Eq e, Show e) => Gen e -> Property
prop_Associativity gen = property $ do
    e1 <- forAll (genParseError gen)
    e2 <- forAll (genParseError gen)
    e3 <- forAll (genParseError gen)
    (e1 <> e2) <> e3 === e1 <> (e2 <> e3)

prop_Idempotency :: (Eq e, Show e) => Gen e -> Property
prop_Idempotency gen = property $ do
    err <- forAll (genParseError gen)
    err <> err === err

prop_monoidMorphism_Unit :: (Eq e, Show e) => (e -> e -> e) -> Gen e -> Property
prop_monoidMorphism_Unit h gen = property $ do
    -- Needed for inference.
    let unit = mempty :: ParseError e

    f <- forAll (genFunctionExp gen)
    fmap (makeFunction h f) unit === unit

prop_monoidMorphism_Mult :: (Eq e, Show e) => (e -> e -> e) -> Gen e -> Property
prop_monoidMorphism_Mult h gen = property $ do
    f <- forAll (genFunctionExp gen)
    e1 <- forAll (genParseError gen)
    e2 <- forAll (genParseError gen)
    fmap (makeFunction h f) (e1 <> e2) === fmap (makeFunction h f) e1 <> fmap (makeFunction h f) e2


-- Main module test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Types.ParseError" [
        ("prop_leftIdentity", prop_leftIdentity gen),
        ("prop_rightIdentity", prop_rightIdentity gen),
        ("prop_Associativity", prop_Associativity gen),
        ("prop_Idempotency", prop_Idempotency gen),
        ("prop_monoidMorphism_Unit", prop_monoidMorphism_Unit max gen),
        ("prop_monoidMorphism_Mult", prop_monoidMorphism_Mult max gen)
        ]
    where
        gen = Gen.word8 Range.linearBounded
