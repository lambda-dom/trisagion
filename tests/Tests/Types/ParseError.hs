{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module Tests.Types.ParseError (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing.
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (Gen, hedgehog, forAll, (===))

-- Base.
import Data.Word (Word32)

-- Libraries.
import Optics.Core ((%), review)

-- Package.
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)


-- Constructors.
makeError
    :: Word                             -- ^ Offset.
    -> Word32                           -- ^ Error tag.
    -> ParseError Word32
makeError offset tag = review (singleton % errorItem) (offset, tag)

-- Generators.
genParseError :: Gen (ParseError Word32)
genParseError = makeError <$> Gen.word Range.linearBounded <*> Gen.word32 Range.linearBounded


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Types.ParseError tests" $ do
    spec_leftIdentity
    spec_rightIdentity
    spec_Associativity
    spec_Idempotency


-- Spec properties.
spec_leftIdentity :: Spec
spec_leftIdentity = describe "Left identity" $ do
    it "Testing on identity" $ do
        let unit = mempty :: ParseError Word32
        unit <> unit `shouldBe` unit

    it "Testing against ParseError with no backtrace." $ hedgehog $ do
        e <- forAll genParseError
        mempty <> e === e

spec_rightIdentity :: Spec
spec_rightIdentity = describe "Right identity" $ do
    it "Testing on identity" $ do
        let unit = mempty :: ParseError Word32
        unit <> unit `shouldBe` unit

    it "Testing against ParseError with no backtrace." $ hedgehog $ do
        e <- forAll genParseError
        e <> mempty === e

spec_Associativity :: Spec
spec_Associativity = describe "Associativity" $ do
    it "Testing against ParseError with no backtrace." $ hedgehog $ do
        e1 <- forAll genParseError
        e2 <- forAll genParseError
        e3 <- forAll genParseError
        (e1 <> e2) <> e3 === e1 <> (e2 <> e3)

spec_Idempotency :: Spec
spec_Idempotency = describe "Idempotency" $ do
    it "Testing against ParseError with no backtrace." $ hedgehog $ do
        err <- forAll genParseError
        err <> err === err
