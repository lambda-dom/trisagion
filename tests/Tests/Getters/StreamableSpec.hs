module Tests.Getters.StreamableSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Lib.Helpers

-- Module to test.
import Trisagion.Getters.Streamable

-- Package.
import Trisagion.Streams.Counter (initialize)
import Trisagion.Types.ParseError (makeParseErrorNoBacktrace)
import Trisagion.Getters.ParseError (ValidationError(..))


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Getters.Streamable tests" $ do
    spec_eoi
    spec_one
    spec_peek
    spec_satisfy
    spec_matchElem
    spec_oneOf


-- Tests.
spec_eoi :: Spec
spec_eoi = describe "eoi tests" $ do
    it "Success case" $ do
        testGetSuccess
            eoi
            "0123" 
            False
            0

    it "End of input case" $ do
        testGetSuccess
            eoi
            ""
            True
            0

spec_one :: Spec
spec_one = describe "one tests" $ do
    it "Success case" $ do
        testGetSuccess
            one
            "0123" 
            '0'
            1

    it "Failure on end of input" $ do
        testGetError
            one
            ""
            (InputError 1)
            0

spec_peek :: Spec
spec_peek = describe "peek tests" $ do
    it "Success case" $ do
        testGetSuccess
            peek
            "0123"
            (Right '0')
            0

    it "Case of end of input" $ do
        testGetSuccess
            peek
            ""
            (Left $ makeParseErrorNoBacktrace (initialize "") (InputError 1))
            0

spec_satisfy :: Spec
spec_satisfy = describe "satisfy tests" $ do
    it "Success case with satisfied predicate" $ do
        testGetSuccess
            (satisfy ('1' /=))
            "0123"
            '0'
            1

    it "Failure case with false predicate" $ do
        testGetError
            (satisfy ('0' /=))
            "0123"
            (Right ValidationError)
            0

    it "Failure case on end of input" $ do
        testGetError
            (satisfy ('0' /=))
            ""
            (Left $ InputError 1)
            0

spec_matchElem :: Spec
spec_matchElem = describe "matchElem tests" $ do
    it "Success case with matching element" $ do
        testGetSuccess
            (matchElem '0')
            "0123"
            '0'
            1

    it "Failure case with non-matching element" $ do
        testGetError
            (matchElem '1')
            "0123"
            (Right $ MatchError '1')
            0

    it "Failure case on end of input" $ do
        testGetError
            (matchElem '0')
            ""
            (Left $ InputError 1)
            0

spec_oneOf :: Spec
spec_oneOf = describe "oneOf tests" $ do
    it "Success case with satisfied element-hood" $ do
        testGetSuccess
            (oneOf "01")
            "0123"
            '0'
            1

    it "Failure case with false element-hood" $ do
        testGetError
            (oneOf "12")
            "0123"
            (Right $ MatchError "12")
            0

    it "Failure case on end of input" $ do
        testGetError
            (oneOf "01")
            ""
            (Left $ InputError 1)
            0
