module Tests.Types.ParseError (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

-- Package.
import Lib.Gen (parseError, word8)
import Lib.Monoid (test_monoid_laws, test_semigroup_idempotency)


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


{- | Test group for the MonoFunctor module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Trisagion.Types.ParseError module."
        [
            test_monoid_laws_parseerror_word8,
            test_idempotency_parseerror_word8
        ]
