module Tests.Parsers.CharSpec (
    -- * Tests.
    spec,
) where

 
-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Testing helpers.
import Lib.Runners

-- Module to test.
import Trisagion.Parsers.Char (Sign (..), lf, cr, spaces, notSpaces, sign, positive, signed)

-- Package.
import Trisagion.Parsers.ParseError (ValidationError (..))
import Trisagion.Parsers.Streamable (InputError (..))


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parsers.Char tests" $ do
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
        testSuccess
            lf
            "\n123"
            '\n'
            1

    it "Failure case with non-matching element" $ do
        testError
            lf
            "0123"
            (Right $ ValidationError '0')
            0

    it "Failure case on end of input" $ do
        testError
            lf
            ""
            (Left $ InputError 1)
            0

spec_cr :: Spec
spec_cr = describe "cr tests" $ do
    it "Success case" $ do
        testSuccess
            cr
            "\r123"
            '\r'
            1

    it "Failure case with non-matching element" $ do
        testError
            cr
            "0123"
            (Right $ ValidationError '0')
            0

    it "Failure case on end of input" $ do
        testError
            cr
            ""
            (Left $ InputError 1)
            0

spec_spaces :: Spec
spec_spaces = describe "spaces tests" $ do
    it "Success cases" $ do
        testSuccess
            spaces
            "  0123"
            "  "
            2

        testSuccess
            spaces
            "  \t 0123"
            "  \t "
            4

    it "Success cases with chars other than space and tab" $ do
        testSuccess
            spaces
            "\v\f\r\n0123"
            "\v\f\r\n"
            4

    it "No leading whitespace" $ do
        testSuccess
            spaces
            "0123"
            ""
            0

    it "Case of no input" $ do
        testSuccess
            spaces
            ""
            ""
            0

spec_notSpaces :: Spec
spec_notSpaces = describe "notSpaces tests" $ do
    it "Success case" $ do
        testSuccess
            notSpaces
            "0123"
            "0123"
            4

    it "Success on leading whitespace" $ do
        testSuccess
            notSpaces
            "    0123"
            ""
            0

    it "Case of no input" $ do
        testSuccess
            notSpaces
            ""
            ""
            0

spec_sign :: Spec
spec_sign = describe "sign tests" $ do
    it "Success cases" $ do
        testSuccess
            sign
            "+123"
            Positive
            1

        testSuccess
            sign
            "-123"
            Negative
            1

    it "Failed validation" $ do
        testError
            sign
            "0123"
            (Right $ ValidationError '0')
            0

    it "Failure case on end of input" $ do
        testError
            sign
            ""
            (Left $ InputError 1)
            0

spec_positive :: Spec
spec_positive = describe "positive tests" $ do
    it "Success cases" $ do
        testSuccess
            positive
            "123"
            123
            3

        testSuccess
            positive
            "1abc"
            1
            1

    it "Leading zeros" $ do
        testSuccess
            positive
            "00123"
            123
            5

    it "Failed validation" $ do
        testError
            positive
            "abc"
            (Right $ ValidationError "")
            0

    it "Failure case on end of input" $ do
        testError
            positive
            ""
            (Left InsufficientInputError)
            0

spec_signed :: Spec
spec_signed = describe "signed tests" $ do
    it "Success cases" $ do
        testSuccess
            (signed positive)
            "123"
            123
            3

        testSuccess
            (signed positive)
            "00123"
            123
            5

        testSuccess
            (signed positive)
            "+123"
            123
            4

        testSuccess
            (signed positive)
            "-123"
            (-123)
            4
