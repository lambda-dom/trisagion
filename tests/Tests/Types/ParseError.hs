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
import Hedgehog (Property, Gen, Group (..), (===), property, forAll, checkParallel)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Base.
import Data.Word (Word32)

-- Libraries.
import Optics.Core ((%), review)

-- Package.
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)
import Data.Maybe (fromMaybe)



-- Function types.
newtype Shift a = Shift a
    deriving stock (Eq, Show, Functor)

-- Basic functions.
functionalize :: Num a => Shift a -> a -> a
functionalize (Shift x) = (x +)


-- Error constructors.
makeError
    :: Word                             -- ^ Offset.
    -> Word32                           -- ^ Error tag.
    -> ParseError Word32
makeError offset tag = review (singleton % errorItem) (offset, tag)

-- Generators.
genParseError :: Gen (ParseError Word32)
genParseError = makeError <$> Gen.word Range.linearBounded <*> Gen.word32 Range.linearBounded

genParseErrorUnit :: Gen (ParseError Word32)
genParseErrorUnit = fromMaybe mempty <$> Gen.maybe genParseError


-- Main module test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.ParseError" [
    ("prop_leftIdentity", prop_leftIdentity),
    ("prop_rightIdentity", prop_rightIdentity),
    ("prop_Associativity", prop_Associativity),
    ("prop_Idempotency", prop_Idempotency),
    ("prop_monoidMorphism_Unit", prop_monoidMorphism_Unit),
    ("prop_monoidMorphism_Mult", prop_monoidMorphism_Mult)
    ]


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
    n <- forAll $ Shift <$> Gen.word32 Range.linearBounded
    fmap (functionalize n) unit === unit

prop_monoidMorphism_Mult :: Property
prop_monoidMorphism_Mult = property $ do
    n <- forAll $ Shift <$> Gen.word32 Range.linearBounded
    e1 <- forAll genParseErrorUnit
    e2 <- forAll genParseErrorUnit
    fmap (functionalize n) (e1 <> e2) === fmap (functionalize n) e1 <> fmap (functionalize n) e2
