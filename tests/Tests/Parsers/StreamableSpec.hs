module Tests.Parsers.StreamableSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Testing helpers.
import Lib.Runners

-- Module to test.
import Trisagion.Parsers.Streamable (eoi, peek, satisfy, matchElem, oneOf)

-- Package.
import Trisagion.Types.ParseError (makeParseError)
import Trisagion.Streams.Counter (initialize)
import Trisagion.Parser (InputError (..))
import Trisagion.Parsers.ParseError (ValidationError (..))


 -- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parsers.Streamable tests" $ do
    spec_eoi
    spec_peek
    spec_satisfy
    spec_matchElem
    spec_oneOf

 
-- Tests.
spec_eoi :: Spec
spec_eoi = describe "eoi tests" $ do
    it "Success case" $ do
        testSuccess
            eoi
            "0123" 
            False
            0

    it "End of input case" $ do
        testSuccess
            eoi
            ""
            True
            0

spec_peek :: Spec
spec_peek = describe "peek tests" $ do
    it "Success case" $ do
        testSuccess
            peek
            "0123"
            (Right '0')
            0

    it "Case of end of input" $ do
        testSuccess
            peek
            ""
            (Left $ makeParseError (initialize "") (InputError 1))
            0

spec_satisfy :: Spec
spec_satisfy = describe "satisfy tests" $ do
    it "Success case with satisfied predicate" $ do
        testSuccess
            (satisfy ('1' /=))
            "0123"
            '0'
            1

    it "Failure case with false predicate" $ do
        testError
            (satisfy ('0' /=))
            "0123"
            (Right $ ValidationError '0')
            0

    it "Failure case on end of input" $ do
        testError
            (satisfy ('0' /=))
            ""
            (Left $ InputError 1)
            0

spec_matchElem :: Spec
spec_matchElem = describe "matchElem tests" $ do
    it "Success case with matching element" $ do
        testSuccess
            (matchElem '0')
            "0123"
            '0'
            1

    it "Failure case with non-matching element" $ do
        testError
            (matchElem '1')
            "0123"
            (Right $ ValidationError '0')
            0

    it "Failure case on end of input" $ do
        testError
            (matchElem '0')
            ""
            (Left $ InputError 1)
            0

spec_oneOf :: Spec
spec_oneOf = describe "oneOf tests" $ do
    it "Success case with satisfied element-hood" $ do
        testSuccess
            (oneOf "01")
            "0123"
            '0'
            1

    it "Failure case with false element-hood" $ do
        testError
            (oneOf "12")
            "0123"
            (Right $ ValidationError '0')
            0

    it "Failure case on end of input" $ do
        testError
            (oneOf "01")
            ""
            (Left $ InputError 1)
            0
