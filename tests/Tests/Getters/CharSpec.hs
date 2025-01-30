module Tests.Getters.CharSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Lib.Helpers

-- Module to test.
import Trisagion.Getters.Char

-- Package.
import Trisagion.Getters.Streamable (MatchError (..), InputError (..))
import Trisagion.Getters.ParseError (ValidationError(..))


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Getters.Char tests" $ do
    spec_lf
    spec_cr
    spec_spaces
    spec_notSpaces
    spec_sign
    spec_positive
    spec_signed


-- Tests.
spec_lf :: Spec
spec_lf = describe "lf tests" $ do
    it "Success case" $ do
        testGetSuccess
            lf
            "\n123"
            '\n'
            1

    it "Failure case with non-matching element" $ do
        testGetError
            lf
            "0123"
            (Right $ MatchError '\n')
            0

    it "Failure case on end of input" $ do
        testGetError
            lf
            ""
            (Left $ InputError 1)
            0

spec_cr :: Spec
spec_cr = describe "cr tests" $ do
    it "Success case" $ do
        testGetSuccess
            cr
            "\r123"
            '\r'
            1

    it "Failure case with non-matching element" $ do
        testGetError
            cr
            "0123"
            (Right $ MatchError '\r')
            0

    it "Failure case on end of input" $ do
        testGetError
            cr
            ""
            (Left $ InputError 1)
            0

spec_spaces :: Spec
spec_spaces = describe "spaces" $ do
    it "Success cases" $ do
        testGetSuccess
            spaces
            "  0123"
            "  "
            2

        testGetSuccess
            spaces
            "  \t 0123"
            "  \t "
            4

    it "Success cases with chars other than space and tab" $ do
        testGetSuccess
            spaces
            "\v\f\r\n0123"
            "\v\f\r\n"
            4

    it "No leading whitespace" $ do
        testGetSuccess
            spaces
            "0123"
            ""
            0

    it "No input" $ do
        testGetSuccess
            spaces
            ""
            ""
            0

spec_notSpaces :: Spec
spec_notSpaces = describe "notSpaces" $ do
    it "Success case" $ do
        testGetSuccess
            notSpaces
            "0123"
            "0123"
            4

    it "Success on leadinhg whitespace" $ do
        testGetSuccess
            notSpaces
            "    0123"
            ""
            0

    it "No input" $ do
        testGetSuccess
            notSpaces
            ""
            ""
            0

spec_sign :: Spec
spec_sign = describe "sign" $ do
    it "Success cases" $ do
        testGetSuccess
            sign
            "+123"
            Positive
            1

        testGetSuccess
            sign
            "-123"
            Negative
            1

    it "Failed validation" $ do
        testGetError
            sign
            "0123"
            (Right ValidationError)
            0

    it "Failure case on end of input" $ do
        testGetError
            sign
            ""
            (Left $ InputError 1)
            0

spec_positive :: Spec
spec_positive = describe "positive" $ do
    it "Success cases" $ do
        testGetSuccess
            positive
            "123"
            123
            3

        testGetSuccess
            positive
            "1abc"
            1
            1

    it "Leading zeros" $ do
        testGetSuccess
            positive
            "00123"
            123
            5

    it "Failed validation" $ do
        testGetError
            sign
            "abc"
            (Right ValidationError)
            0

    it "Failure case on end of input" $ do
        testGetError
            positive
            ""
            (Left InsufficientInputError)
            0

spec_signed :: Spec
spec_signed = describe "signed" $ do
    it "Success cases" $ do
        testGetSuccess
            (signed positive)
            "123"
            123
            3

        testGetSuccess
            (signed positive)
            "+123"
            123
            4

        testGetSuccess
            (signed positive)
            "-123"
            (-123)
            4
