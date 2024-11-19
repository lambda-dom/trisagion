module Tests.Getters.SplittableSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Tests.Helpers

-- Module to test.
import Trisagion.Getters.Splittable

-- Package.
import Trisagion.Getters.Streamable (InputError (..), MatchError (..), ValidationError (..))


-- Main module test driver.
spec :: Spec
spec = describe "Bins.Getters.Splittable tests" $ do
    spec_takePrefix
    spec_dropPrefix
    spec_takeExact
    spec_match
    spec_takeWith
    spec_atLeastOneWith


-- Tests.
spec_takePrefix :: Spec
spec_takePrefix = describe "takePrefix" $ do
    it "Success case" $ do
        testSuccess
            (takePrefix 2)
            "0123"
            "01"
            "23"

    it "Case of insufficient input" $ do
        testSuccess
            (takePrefix 10)
            "0123"
            "0123"
            ""

spec_dropPrefix :: Spec
spec_dropPrefix = describe "dropPrefix" $ do
    it "Success case" $ do
        testSuccess
            (dropPrefix 2)
            "0123"
            ()
            "23"

    it "Case of insufficient input" $ do
        testSuccess
            (dropPrefix 10)
            "0123"
            ()
            ""

spec_takeExact :: Spec
spec_takeExact = describe "takeExact" $ do
    it "Success case" $ do
        testSuccess
            (takeExact 2)
            "0123"
            "01"
            "23"

    it "Case of insufficient input" $ do
        testError
            (takeExact 10)
            "0123"
            "0123"
            (InputError 10)

spec_match :: Spec
spec_match = describe "match" $ do
    it "Success case" $ do
        testSuccess
            (match "01")
            "0123"
            "01"
            "23"

    it "Failure on insufficient input" $ do
        testError
            (match "01234")
            "0123"
            "0123"
            (Left $ InputError 5)

    it "Failure on failing match" $ do
        testError
            (match "{{}}")
            "0123"
            "0123"
            (Right (MatchError "{{}}"))

spec_takeWith :: Spec
spec_takeWith = describe "takeWith" $ do
    it "Success case" $ do
        testSuccess
            (takeWith ('3' /=))
            "0123"
            "012"
            "3"

    it "No input case" $ do
        testSuccess
            (takeWith ('3' /=))
            ""
            ""
            ""

spec_atLeastOneWith :: Spec
spec_atLeastOneWith = describe "atLeastOneWith" $ do
    it "Success cases" $ do
         testSuccess
            (atLeastOneWith ('0' ==))
            "0123"
            "0"
            "123"

         testSuccess
            (atLeastOneWith ('0' ==))
            "0003"
            "000"
            "3"

    it "Failed validation" $ do
        testError
            (atLeastOneWith ('1' ==))
            "0123"
            "0123"
            (Right ValidationError)

    it "Failure on empty input" $ do
        testError
            (atLeastOneWith ('1' ==))
            ""
            ""
            (Left InsufficientInputError)
