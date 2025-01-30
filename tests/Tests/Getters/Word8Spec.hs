module Tests.Getters.Word8Spec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Lib.Helpers

-- Module to test.
import Trisagion.Getters.Word8

-- Package.
import Trisagion.Getters.Streamable (InputError(..))
import Data.Int (Int8)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Getters.Word8 tests" $ do
    spec_word8
    spec_int8
    spec_word32Le
    spec_word32Be


spec_word8 :: Spec
spec_word8 = describe "word8 tests" $ do
    it "Success case" $ do
        testGetSuccess
            word8
            [0, 1, 2, 3] 
            0
            1

    it "Failure on end of input" $ do
        testGetError
            word8
            []
            (InputError 1)
            0

spec_int8 :: Spec
spec_int8 = describe "int8 tests" $ do
    it "Success case" $ do
        testGetSuccess
            int8
            [0, 1, 2, 3] 
            0
            1

    it "Success case with fromIntegral" $ do
        testGetSuccess
            int8
            [fromIntegral (-1 :: Int8), 1, 2, 3] 
            (-1)
            1

    it "Failure on end of input" $ do
        testGetError
            int8
            []
            (InputError 1)
            0

spec_word32Le :: Spec
spec_word32Le = describe "word32Le tests" $ do
    it "Success cases" $ do
       testGetSuccess
            word32Le
            [0, 0, 0, 0, 0, 0, 0, 0] 
            0
            4
       testGetSuccess
            word32Le
            [1, 0, 0, 0, 0, 0, 0, 0] 
            1
            4

       testGetSuccess
            word32Le
            [0, 1, 0, 0, 0, 0, 0, 0] 
            256
            4

       testGetSuccess
            word32Le
            [0, 0, 1, 0, 0, 0, 0, 0] 
            65536
            4

       testGetSuccess
            word32Le
            [0, 0, 0, 1, 0, 0, 0, 0] 
            16777216
            4

    it "Failure on end of input" $ do
        testGetError
            word32Le
            [0, 0, 0]
            (InputError 4)
            0

spec_word32Be :: Spec
spec_word32Be = describe "Tests for word32Be" $ do
    it "Success cases" $ do
       testGetSuccess
            word32Be
            [0, 0, 0, 0, 0, 0, 0, 0] 
            0
            4

       testGetSuccess
            word32Be
            [0, 0, 0, 1, 0, 0, 0, 0] 
            1
            4

       testGetSuccess
            word32Be
            [0, 0, 1, 0, 0, 0, 0, 0] 
            256
            4

       testGetSuccess
            word32Be
            [0, 1, 0, 0, 0, 0, 0, 0] 
            65536
            4

       testGetSuccess
            word32Be
            [1, 0, 0, 0, 0, 0, 0, 0] 
            16777216
            4

    it "Failure on end of input" $ do
        testGetError
            word32Be
            [0, 0, 0]
            (InputError 4)
            0
