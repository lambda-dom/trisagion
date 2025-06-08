{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Types.ParseError (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Hedgehog (Property, Group (..), (===), property, forAll, checkParallel)

-- Base.
import Data.Word (Word32)

-- Package.
import Trisagion.Types.ParseError (ParseError)

-- Package testing helpers.
import Lib.Generators (genParseErrorUnit, genShift, makeFunction)


-- Properties.
prop_leftIdentity :: Property
prop_leftIdentity = property $ do
    e <- forAll genParseErrorUnit
    mempty <> e === e

prop_rightIdentity :: Property
prop_rightIdentity = property $ do
    e <- forAll genParseErrorUnit
    e <> mempty === e

prop_Associativity :: Property
prop_Associativity = property $ do
    e1 <- forAll genParseErrorUnit
    e2 <- forAll genParseErrorUnit
    e3 <- forAll genParseErrorUnit
    (e1 <> e2) <> e3 === e1 <> (e2 <> e3)

prop_Idempotency :: Property
prop_Idempotency = property $ do
    err <- forAll genParseErrorUnit
    err <> err === err

prop_monoidMorphism_Unit :: Property
prop_monoidMorphism_Unit = property $ do
    let unit = mempty :: ParseError Word32
    n <- forAll genShift
    fmap (makeFunction n) unit === unit

prop_monoidMorphism_Mult :: Property
prop_monoidMorphism_Mult = property $ do
    n <- forAll genShift
    e1 <- forAll genParseErrorUnit
    e2 <- forAll genParseErrorUnit
    fmap (makeFunction n :: Word32 -> Word32) (e1 <> e2) === fmap (makeFunction n) e1 <> fmap (makeFunction n) e2


-- Main module test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Types.ParseError" [
    ("prop_leftIdentity", prop_leftIdentity),
    ("prop_rightIdentity", prop_rightIdentity),
    ("prop_Associativity", prop_Associativity),
    ("prop_Idempotency", prop_Idempotency),
    ("prop_monoidMorphism_Unit", prop_monoidMorphism_Unit),
    ("prop_monoidMorphism_Mult", prop_monoidMorphism_Mult)
    ]
