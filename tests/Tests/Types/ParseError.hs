{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Types.ParseError (
    -- * Tests.
    tests,

    -- * Properties.
    prop_fmap_monoid_morphism_unit,
    prop_fmap_monoid_morphism_mult,
    fmapMonoidMorphismLaws,
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty (..))

-- Testing library.
import Hedgehog (PropertyT, Gen, Property, Group (..), checkParallel, forAll, property)
import qualified Hedgehog.Gen as Gen (word8)
import qualified Hedgehog.Range as Range (linearBounded)

-- Package.
import Trisagion.Lib.Utils (withBinary)
import Trisagion.Types.ParseError (ParseError)

-- Package testing.
import Lib.Generators (parseErrors)
import Lib.Function (Function, functions, fromFunction)
import Lib.Property (makeGroup, andM)
import Lib.Properties.Monoid (
    monoidLaws,
    prop_monoid_morphism_mult,
    prop_monoid_morphism_unit,
    prop_monoid_idempotency
    )


-- Properties.
prop_fmap_monoid_morphism_unit
    :: forall m a . (Monad m, Ord a, Show a)
    => Gen a
    -> PropertyT m ()
prop_fmap_monoid_morphism_unit xs = do
        f <- nat <$> forAll (functions xs xs)
        prop_monoid_morphism_unit (transf f)
    where
        nat :: Function a a -> a -> a
        nat = fromFunction (id :| []) ((.) :| (withBinary <$> [min, max]))

        transf :: (a -> a) -> ParseError a -> ParseError a
        transf = fmap

prop_fmap_monoid_morphism_mult
    :: forall m a . (Monad m, Ord a, Show a)
    => Gen a
    -> PropertyT m ()
prop_fmap_monoid_morphism_mult elems = do
        f <- nat <$> forAll (functions elems elems)
        prop_monoid_morphism_mult (fmap f) (parseErrors 10 elems)
    where
        nat :: Function a a -> a -> a
        nat = fromFunction (id :| []) ((.) :| (withBinary <$> [min, max]))

fmapMonoidMorphismLaws
    :: forall a . (Ord a, Show a)
    => Gen a
    -> [(String, Property)]
fmapMonoidMorphismLaws elems = fmap property <$> props
    where
        props = [
            ("Monoid morphism unit", prop_fmap_monoid_morphism_unit elems),
            ("Monoid morphism multiplication", prop_fmap_monoid_morphism_mult elems)
            ]

-- Property groups.
testMonoidLaws :: Group
testMonoidLaws =
    makeGroup
        "Monoid laws for ParseError"
        (monoidLaws $ parseErrors 10 (Gen.word8 Range.linearBounded))

testMonoidIdempotency :: Group
testMonoidIdempotency =
        makeGroup
            "Idempotency of ParseError monoid"
            [("Idempotency", property $ prop_monoid_idempotency gen)]
    where
        gen = parseErrors 10 (Gen.word8 Range.linearBounded)

testMonoidMorphismLaws :: Group
testMonoidMorphismLaws =
    makeGroup
        "Monoid morphism laws for @fmap f@"
        (fmapMonoidMorphismLaws $ Gen.word8 Range.linearBounded)

-- Main test driver.
tests :: IO Bool
tests = andM (checkParallel <$> [testMonoidLaws, testMonoidIdempotency, testMonoidMorphismLaws])
