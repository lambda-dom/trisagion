module Tests.Parsers.ParseErrorSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Testing helpers.
import Lib.Runners

-- Module to test.
import Trisagion.Parsers.ParseError (validate, failIff)
import qualified Trisagion.Parsers.ParseError as Parsers (until)

-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Package.
import Trisagion.Parser (InputError (..), one)
import Trisagion.Parsers.Streamable (matchElem)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parsers.ParseError tests" $ do
    spec_validate
    spec_failIff
    spec_until


-- Tests.
spec_validate :: Spec
spec_validate = describe "validate tests" $ do
    it "Success case" $ do
        testSuccess
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            "0123"
            '0'
            1

    it "Case of parser failure on end of input" $ do
         testError
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            ""
            (Left $ InputError 1)
            0

    it "Case of failed validation" $ do
         testError
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            "123"
            (Right ())
            0

spec_failIff :: Spec
spec_failIff = describe "failIff tests" $ do
    it "Success case" $ do
        testSuccess
            (failIff $ matchElem '1')
            "0123"
            ()
            0

    it "Failure case" $ do
        testFail
            (failIff $ matchElem '0')
            "0123"

    it "End of input case" $ do
        testSuccess
            (failIff $ matchElem '0')
            ""
            ()
            0

spec_until :: Spec
spec_until = describe "until tests" $ do
    it "Success case" $ do
        testSuccess
            (Parsers.until (matchElem '}') (first (fmap Left) one))
            "01}3"
            "01"
            2

    it "Case of closing parser succeeding on head of input" $ do
        testSuccess
            (Parsers.until (matchElem '}') (first (fmap Left) one))
            "}123"
            ""
            0
