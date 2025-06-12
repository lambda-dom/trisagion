{-# LANGUAGE OverloadedStrings #-}

module Tests.Types.ParseError (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Maybe (fromMaybe)

-- Libraries.
import Optics.Core ((%), review)

-- Testing.
import Hedgehog (Gen, Group (..), property, checkParallel)
import qualified Hedgehog.Range as Range (linearBounded)
import qualified Hedgehog.Gen as Gen (word8, maybe)

-- Package.
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)

-- Package testing helpers.
import Lib.Generators (genSize)
import Lib.Properties (
    prop_monoid_left_identity,
    prop_monoid_right_identity,
    prop_monoid_associativity,
    prop_monoid_idempotency,
    )


{- | Generate 'ParseError' errors with no backtrace. -}
genParseError :: Gen e -> Gen (ParseError e)
genParseError gen = fromMaybe mempty <$> Gen.maybe genError
    where
        makeError offset tag = review (singleton % errorItem) (offset, tag)
        genError = makeError <$> genSize <*> gen

-- Properties.
-- prop_monoidMorphism_Unit :: (Eq e, Show e) => (e -> e -> e) -> Gen e -> Property
-- prop_monoidMorphism_Unit h gen = property $ do
--     -- Needed for inference.
--     let unit = mempty :: ParseError e

--     f <- forAll (genFunctionExp gen)
--     fmap (makeFunction h f) unit === unit

-- prop_monoidMorphism_Mult :: (Eq e, Show e) => (e -> e -> e) -> Gen e -> Property
-- prop_monoidMorphism_Mult h gen = property $ do
--     f <- forAll (genFunctionExp gen)
--     e1 <- forAll (genParseError gen)
--     e2 <- forAll (genParseError gen)
--     fmap (makeFunction h f) (e1 <> e2) === fmap (makeFunction h f) e1 <> fmap (makeFunction h f) e2


-- Main module test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Types.ParseError" [
        ("Left monoid identity", property $ prop_monoid_left_identity gen),
        ("Right monoid identity", property $ prop_monoid_right_identity gen),
        ("Associativity", property $ prop_monoid_associativity gen),
        ("Idempotency", property $ prop_monoid_idempotency gen)
        ]
    where
        gen = genParseError $ Gen.word8 Range.linearBounded
