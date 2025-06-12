{-# LANGUAGE OverloadedStrings #-}

module Tests.Types.ParseError (
    -- * Tests.
    tests,

    -- * Properties.
    prop_fmap_monoid_morphism_unit,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

-- Libraries.
import Optics.Core ((%), review)

-- Testing.
import Hedgehog (Gen, Group (..), property, checkParallel, PropertyT, forAll)
import qualified Hedgehog.Range as Range (linearBounded)
import qualified Hedgehog.Gen as Gen (word8, maybe)

-- Package.
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
    )


{- | Generate 'ParseError' errors with no backtrace. -}
genParseError :: Gen e -> Gen (ParseError e)
genParseError gen = fromMaybe mempty <$> Gen.maybe genError
    where
        makeError offset tag = review (singleton % errorItem) (offset, tag)
        genError = makeError <$> genSize <*> gen

-- Properties.
prop_fmap_monoid_morphism_unit
    :: forall m e . (Monad m, Show e, Ord e)
    => Gen e
    -> PropertyT m ()
prop_fmap_monoid_morphism_unit gen = do
        f <- forAll genF
        prop_monoid_morphism_unit (transf f)
    where
        genF :: Gen (Function e e)
        genF = genFunction gen gen

        combine :: (b -> b -> b) -> (a -> b) -> (a -> b) -> (a -> b)
        combine h f g = uncurry h . bimap f g . (\ x -> (x, x))

        nat :: Function e e -> e -> e
        nat = fromFunction (id :| []) ((.) :| [combine min, combine max])

        transf :: Function e e -> ParseError e -> ParseError e
        transf g = fmap (nat g)

-- prop_monoidMorphism_Mult :: (Eq e, Show e) => (e -> e -> e) -> Gen e -> Property
-- prop_monoidMorphism_Mult h gen = property $ do
--     f <- forAll (genFunctionExp gen)
--     e1 <- forAll (genParseError gen)
--     e2 <- forAll (genParseError gen)
--     fmap (makeFunction h f) (e1 <> e2) === fmap (makeFunction h f) e1 <> fmap (makeFunction h f) e2


-- Main module test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Types.ParseError" [
        ("Left monoid identity", property $ prop_monoid_left_identity genP),
        ("Right monoid identity", property $ prop_monoid_right_identity genP),
        ("Associativity", property $ prop_monoid_associativity genP),
        ("Idempotency", property $ prop_monoid_idempotency genP),
        ("Monoid morphism for functor map: unit", property $ prop_fmap_monoid_morphism_unit genW)
        ]
    where
        genW :: Gen Word8
        genW = Gen.word8 Range.linearBounded

        genP :: Gen (ParseError Word8)
        genP = genParseError genW
