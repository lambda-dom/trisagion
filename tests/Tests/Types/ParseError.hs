{-# LANGUAGE OverloadedStrings #-}

module Tests.Types.ParseError (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

-- Libraries.
import Optics.Core ((%), review)

-- Testing.
import Hedgehog (PropertyT, Gen, Group (..), property, checkParallel, forAll)
import qualified Hedgehog.Range as Range (linearBounded)
import qualified Hedgehog.Gen as Gen (word8, maybe)

-- Package.
import Trisagion.Lib.Utils (withBinary)
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)

-- Package testing helpers.
import Lib.Functions (Function, genFunction, fromFunction)
import Lib.Generators (genSize)
import Lib.Properties (
    prop_monoid_left_identity,
    prop_monoid_right_identity,
    prop_monoid_associativity,
    prop_monoid_idempotency,
    prop_monoid_morphism_unit,
    prop_function_extensional_equality,
    )


-- Properties.
prop_fmap_monoid_morphism_unit
    :: forall m e . (Monad m, Show e, Ord e)
    => Gen e
    -> PropertyT m ()
prop_fmap_monoid_morphism_unit gen = do
        f <- forAll (genFunction gen gen)
        prop_monoid_morphism_unit (t f)
    where
        nat :: Function e e -> e -> e
        nat = fromFunction (id :| []) ((.) :| (withBinary <$> [min, max]))

        t :: Function e e -> ParseError e -> ParseError e
        t f = fmap (nat f)

prop_fmap_monoid_morphism_mult
    :: forall m e . (Monad m, Show e, Ord e)
    => Gen e
    -> PropertyT m ()
prop_fmap_monoid_morphism_mult gen = do
        f <- forAll (genFunction gen gen)
        prop_function_extensional_equality (tLeft f) (tRight f) pairs
    where
        pairs :: Gen (ParseError e, ParseError e)
        pairs = (,) <$> genParseError gen <*> genParseError gen

        nat :: Function e e -> e -> e
        nat = fromFunction (id :| []) ((.) :| (withBinary <$> [min, max]))

        tLeft :: Function e e -> (ParseError e, ParseError e) -> ParseError e
        tLeft f = fmap (nat f) . uncurry (<>)

        tRight :: Function e e -> (ParseError e, ParseError e) -> ParseError e
        tRight f = uncurry (<>) . bimap (fmap (nat f)) (fmap (nat f))


-- Main module test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Types.ParseError" [
        ("Left monoid identity", property $ prop_monoid_left_identity genP),
        ("Right monoid identity", property $ prop_monoid_right_identity genP),
        ("Associativity", property $ prop_monoid_associativity genP),
        ("Idempotency", property $ prop_monoid_idempotency genP),
        ("Monoid morphism for fmap: unit", property $ prop_fmap_monoid_morphism_unit genW),
        ("Monoid morphism for fmap: mult", property $ prop_fmap_monoid_morphism_mult genW)
        ]
    where
        genW :: Gen Word8
        genW = Gen.word8 Range.linearBounded

        genP :: Gen (ParseError Word8)
        genP = genParseError genW
