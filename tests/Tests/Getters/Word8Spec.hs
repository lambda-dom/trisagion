module Tests.Getters.Word8Spec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Tests.Helpers

-- Module to test.
import Trisagion.Getters.Word8

-- Base.
import Data.Int (Int8)

-- Package.
import Trisagion.Getters.Streamable (InputError (..))


-- Main module test driver.
spec :: Spec
spec = describe "Bins.Getters.Streamable tests" $ do
    spec_word8
    spec_int8
    spec_word32Le
    spec_word32Be


-- Tests.
spec_word8 :: Spec
spec_word8 = describe "word8" $ do
    it "Success case" $ do
        testSuccess
            word8
            [0, 1, 2, 3] 
            0
            [1, 2, 3]

    it "Failure on end of input" $ do
        testError
            word8
            []
            []
            (InputError 1)

spec_int8 :: Spec
spec_int8 = describe "int8" $ do
    it "Success case" $ do
        testSuccess
            int8
            [0, 1, 2, 3] 
            0
            [1, 2, 3]

    it "Success case with fromIntegral" $ do
        testSuccess
            int8
            [fromIntegral (-1 :: Int8), 1, 2, 3] 
            (-1)
            [1, 2, 3]

    it "Failure on end of input" $ do
        testError
            int8
            []
            []
            (InputError 1)

spec_word32Le :: Spec
spec_word32Le = describe "Tests for word32Le" $ do
    it "Success cases" $ do
       testSuccess
            word32Le
            [0, 0, 0, 0, 0, 0, 0, 0] 
            0
            [0, 0, 0, 0]

       testSuccess
            word32Le
            [1, 0, 0, 0, 0, 0, 0, 0] 
            1
            [0, 0, 0, 0]

       testSuccess
            word32Le
            [0, 1, 0, 0, 0, 0, 0, 0] 
            256
            [0, 0, 0, 0]

       testSuccess
            word32Le
            [0, 0, 1, 0, 0, 0, 0, 0] 
            65536
            [0, 0, 0, 0]

       testSuccess
            word32Le
            [0, 0, 0, 1, 0, 0, 0, 0] 
            16777216
            [0, 0, 0, 0]

    it "Failure on end of input" $ do
        testError
            word32Le
            [0, 0, 0]
            [0, 0, 0]
            (InputError 4)

spec_word32Be :: Spec
spec_word32Be = describe "Tests for word32Be" $ do
    it "Success cases" $ do
       testSuccess
            word32Be
            [0, 0, 0, 0, 0, 0, 0, 0] 
            0
            [0, 0, 0, 0]

       testSuccess
            word32Be
            [0, 0, 0, 1, 0, 0, 0, 0] 
            1
            [0, 0, 0, 0]

       testSuccess
            word32Be
            [0, 0, 1, 0, 0, 0, 0, 0] 
            256
            [0, 0, 0, 0]

       testSuccess
            word32Be
            [0, 1, 0, 0, 0, 0, 0, 0] 
            65536
            [0, 0, 0, 0]

       testSuccess
            word32Be
            [1, 0, 0, 0, 0, 0, 0, 0] 
            16777216
            [0, 0, 0, 0]

    it "Failure on end of input" $ do
        testError
            word32Be
            [0, 0, 0]
            [0, 0, 0]
            (InputError 4)
