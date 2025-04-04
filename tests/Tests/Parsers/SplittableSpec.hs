module Tests.Parsers.SplittableSpec (
    -- * Tests.
    spec,
) where

 
-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Testing helpers.
import Lib.Runners

-- Module to test.
import Trisagion.Parsers.Splittable (dropPrefix, takeExact, match, atLeastOneWith)

-- Package.
import Trisagion.Parser (InputError (..))
import Trisagion.Parsers.ParseError (ValidationError (..))


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parsers.Splittable tests" $ do
    spec_dropPrefix
    spec_takeExact
    spec_match
    spec_atLeastOneWith


-- Tests.
spec_dropPrefix :: Spec
spec_dropPrefix = describe "dropPrefix tests" $ do
    it "Success case" $ do
        testSuccess
            (dropPrefix 2)
            "0123"
            ()
            2

    it "Case of insufficient input" $ do
        testSuccess
            (dropPrefix 10)
            "0123"
            ()
            4

spec_takeExact :: Spec
spec_takeExact = describe "takeExact tests" $ do
    it "Success case" $ do
        testSuccess
            (takeExact 2)
            "0123"
            "01"
            2

    it "Case of insufficient input" $ do
        testError
            (takeExact 10)
            "0123"
            (InputError 10)
            0

spec_match :: Spec
spec_match = describe "match tests" $ do
    it "Success case" $ do
        testSuccess
            (match "01")
            "0123"
            "01"
            2

    it "Failure on insufficient input" $ do
        testError
            (match "01234")
            "0123"
            (Left $ InputError 5)
            0

    it "Failure on failing match" $ do
        testError
            (match "{{}}")
            "0123"
            (Right $ ValidationError "0123")
            0

spec_atLeastOneWith :: Spec
spec_atLeastOneWith = describe "atLeastOneWith" $ do
    it "Success cases" $ do
         testSuccess
            (atLeastOneWith ('0' ==))
            "0123"
            "0"
            1

         testSuccess
            (atLeastOneWith ('0' ==))
            "0003"
            "000"
            3

    it "Failed validation" $ do
        testError
            (atLeastOneWith ('1' ==))
            "0123"
            (Right $ ValidationError "")
            0

    it "Failure on empty input" $ do
        testError
            (atLeastOneWith ('1' ==))
            ""
            (Left InsufficientInputError)
            0
