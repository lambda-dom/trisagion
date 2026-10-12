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
import Lib.Monoid (test_monoid_laws)


{- | Test the @MonoFunctor@ laws for the @ByteString@ instance. -}
test_monoid_laws_word8 :: TestTree
test_monoid_laws_word8 =
    testProperty
        "Test Monoid laws of the ParseError Word8 instance."
        (test_monoid_laws . parseError $ word8)


{- | Test group for the MonoFunctor module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Trisagion.Types.ParseError module."
        [
            test_monoid_laws_word8
        ]
