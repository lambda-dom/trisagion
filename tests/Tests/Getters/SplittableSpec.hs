module Tests.Getters.SplittableSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Lib.Helpers

-- Module to test.
import Trisagion.Getters.Splittable

-- Package.
import Trisagion.Getters.ParseError (ValidationError (..))
import Trisagion.Getters.Streamable (InputError (..), MatchError (..))


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Getters.Splittable tests" $ do
    spec_takePrefix
    spec_dropPrefix
    spec_takeExact
    spec_match
    spec_takeWith
    spec_atLeastOneWith


-- Tests.
spec_takePrefix :: Spec
spec_takePrefix = describe "takePrefix tests" $ do
    it "Success case" $ do
        testGetSuccess
            (takePrefix 2)
            "0123"
            "01"
            2

    it "Case of insufficient input" $ do
        testGetSuccess
            (takePrefix 10)
            "0123"
            "0123"
            4

spec_dropPrefix :: Spec
spec_dropPrefix = describe "dropPrefix tests" $ do
    it "Success case" $ do
        testGetSuccess
            (dropPrefix 2)
            "0123"
            ()
            2

    it "Case of insufficient input" $ do
        testGetSuccess
            (dropPrefix 10)
            "0123"
            ()
            4

spec_takeExact :: Spec
spec_takeExact = describe "takeExact tests" $ do
    it "Success case" $ do
        testGetSuccess
            (takeExact 2)
            "0123"
            "01"
            2

    it "Case of insufficient input" $ do
        testGetError
            (takeExact 10)
            "0123"
            (InputError 10)
            0

spec_match :: Spec
spec_match = describe "match tests" $ do
    it "Success case" $ do
        testGetSuccess
            (match "01")
            "0123"
            "01"
            2

    it "Failure on insufficient input" $ do
        testGetError
            (match "01234")
            "0123"
            (Left $ InputError 5)
            0

    it "Failure on failing match" $ do
        testGetError
            (match "{{}}")
            "0123"
            (Right (MatchError "{{}}"))
            0

spec_takeWith :: Spec
spec_takeWith = describe "takeWith" $ do
    it "Success case" $ do
        testGetSuccess
            (takeWith ('3' /=))
            "0123"
            "012"
            3

    it "No input case" $ do
        testGetSuccess
            (takeWith ('3' /=))
            ""
            ""
            0

spec_atLeastOneWith :: Spec
spec_atLeastOneWith = describe "atLeastOneWith" $ do
    it "Success cases" $ do
         testGetSuccess
            (atLeastOneWith ('0' ==))
            "0123"
            "0"
            1

         testGetSuccess
            (atLeastOneWith ('0' ==))
            "0003"
            "000"
            3

    it "Failed validation" $ do
        testGetError
            (atLeastOneWith ('1' ==))
            "0123"
            (Right ValidationError)
            0

    it "Failure on empty input" $ do
        testGetError
            (atLeastOneWith ('1' ==))
            ""
            (Left InsufficientInputError)
            0
