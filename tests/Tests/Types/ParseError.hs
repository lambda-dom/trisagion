{-# LANGUAGE PatternSynonyms #-}

module Tests.Types.ParseError (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Word (Word8)

-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Falsify (Property, Fun, pattern Fn, gen)
import Test.Falsify.Generator (fun)

-- Package.
import Lib.Gen (parseError, word8)
import Lib.Monoid (test_monoid_laws, test_semigroup_idempotency, test_monoid_morphism_law)


{- | Test the monoid laws for the @'ParseError' Word8@ instance. -}
test_monoid_laws_parseerror_word8 :: TestTree
test_monoid_laws_parseerror_word8 =
    testProperty
        "Test Monoid laws of the ParseError Word8 instance."
        (test_monoid_laws . parseError $ word8)

{- | Test idempotency of @'ParseError' Word8@. -}
test_idempotency_parseerror_word8 :: TestTree
test_idempotency_parseerror_word8 =
    testProperty
        "Test idempotency of the ParseError Word8 instance."
        (test_semigroup_idempotency . parseError $ word8)

{- | Test monoid morphism laws for @fmap f@ for all @f :: 'ParseError' Word8 -> 'ParseError' Word8@. -}
test_monoid_morphism_fmap_int :: TestTree
test_monoid_morphism_fmap_int =
        testProperty
            "Test monoid morphism laws for all fmap f in the ParseError Word8 case."
            (test_fmap)
    where
        test_fmap :: Property ()
        test_fmap = do
            (f :: Fun Word8 Word8) <- gen $ fun word8
            let
                Fn f' = f
            test_monoid_morphism_law
                ("fmap " ++ show f, fmap f')
                (parseError word8)

{- | Test group for the MonoFunctor module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Trisagion.Types.ParseError module."
        [
            test_monoid_laws_parseerror_word8,
            test_idempotency_parseerror_word8,
            test_monoid_morphism_fmap_int
        ]
