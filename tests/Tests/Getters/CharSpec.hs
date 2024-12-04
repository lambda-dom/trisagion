module Tests.Getters.CharSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Tests.Helpers

-- Module to test.
import Trisagion.Getters.Char

-- Package.
import Trisagion.Getters.Streamable (InputError (..), MatchError (..), ValidationError (..))


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Getters.Char tests" $ do
    spec_lf
    spec_cr
    spec_spaces
    spec_sign
    spec_positive
    spec_signed


spec_lf :: Spec
spec_lf = describe "lf" $ do
    it "Success case" $ do
        testSuccess
            lf
            "\n123"
            '\n'
            "123"

    it "Failure case with non-matching element" $ do
        testError
            lf
            "0123"
            "0123"
            (Right $ MatchError '\n')

    it "Failure case on end of input" $ do
        testError
            lf
            ""
            ""
            (Left $ InputError 1)

spec_cr :: Spec
spec_cr = describe "cr" $ do
    it "Success case" $ do
        testSuccess
            cr
            "\r123"
            '\r'
            "123"

    it "Failure case with non-matching element" $ do
        testError
            cr
            "0123"
            "0123"
            (Right $ MatchError '\r')

    it "Failure case on end of input" $ do
        testError
            cr
            ""
            ""
            (Left $ InputError 1)

spec_spaces :: Spec
spec_spaces = describe "spaces" $ do
    it "Success cases" $ do
        testSuccess
            spaces
            "  0123"
            "  "
            "0123"

        testSuccess
            spaces
            "  \t 0123"
            "  \t "
            "0123"

    it "Success cases with chars other than space and tab" $ do
        testSuccess
            spaces
            "\v\f\r\n0123"
            "\v\f\r\n"
            "0123"

    it "No leading whitespace" $ do
        testSuccess
            spaces
            "0123"
            ""
            "0123"

    it "No input" $ do
        testSuccess
            spaces
            ""
            ""
            ""

spec_sign :: Spec
spec_sign = describe "sign" $ do
    it "Success cases" $ do
        testSuccess
            sign
            "+123"
            Positive
            "123"

        testSuccess
            sign
            "-123"
            Negative
            "123"

    it "Failed validation" $ do
        testError
            sign
            "0123"
            "0123"
            (Right ValidationError)

    it "Failure case on end of input" $ do
        testError
            sign
            ""
            ""
            (Left $ InputError 1)

spec_positive :: Spec
spec_positive = describe "positive" $ do
    it "Success cases" $ do
        testSuccess
            positive
            "123"
            123
            ""

        testSuccess
            positive
            "1abc"
            1
            "abc"

    it "Leading zeros" $ do
        testSuccess
            positive
            "00123"
            123
            ""

    it "Failed validation" $ do
        testError
            sign
            "abc"
            "abc"
            (Right ValidationError)

    it "Failure case on end of input" $ do
        testError
            positive
            ""
            ""
            (Left InsufficientInputError)

spec_signed :: Spec
spec_signed = describe "signed" $ do
    it "Success cases" $ do
        testSuccess
            (signed positive)
            "123"
            123
            ""

        testSuccess
            (signed positive)
            "+123"
            123
            ""

        testSuccess
            (signed positive)
            "-123"
            (-123)
            ""
