module Tests.Getters.ParseErrorSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Lib.Helpers

-- Module to test.
import Trisagion.Getters.ParseError
import qualified Trisagion.Getters.ParseError as Getters (until)

-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Package.
import Trisagion.Getters.Streamable (InputError (..), MatchError (..), one, matchElem)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Getters.ParseError tests" $ do
    spec_validate
    spec_failIff
    spec_until


-- Tests.
spec_validate :: Spec
spec_validate = describe "validate tests" $ do
    it "Success case" $ do
        testGetSuccess
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            "0123"
            '0'
            1

    it "Case of parser failure on end of input" $ do
         testGetError
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            ""
            (Left $ InputError 1)
            0

    it "Case of parser failure but parser position advanced" $ do
         testGetError
            (validate
                (\ c -> if c == '0' then Right c else Left ())
                (first (fmap Left) one *> matchElem '1'))
            "023"
            (Left $ Right $ MatchError '1')
            1

    it "Case of failed validation but parser state not advanced" $ do
         testGetError
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            "123"
            (Right ())
            0

spec_failIff :: Spec
spec_failIff = describe "failIff tests" $ do
    it "Success case" $ do
        testGetSuccess
            (failIff $ matchElem '1')
            "0123"
            ()
            0

    it "Failure case" $ do
        testGetFail
            (failIff $ matchElem '0')
            "0123"

    it "End of input case" $ do
        testGetSuccess
            (failIff $ matchElem '0')
            ""
            ()
            0

spec_until :: Spec
spec_until = describe "until tests" $ do
    it "Success case" $ do
        testGetSuccess
            (Getters.until (matchElem '}') (first (fmap Left) one))
            "01}3"
            "01"
            2

    it "Case of failure of first parser" $ do
        testGetSuccess
            (Getters.until (matchElem '}') (first (fmap Left) one))
            "}123"
            ""
            0
