module Tests.Parsers.Word8Spec (
    -- * Tests.
    spec,
) where

 
-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Testing helpers.
import Lib.Runners

-- Module to test.
import Trisagion.Parsers.Word8 (word8, int8, word32Le, word32Be)

-- Base.
import Data.Int (Int8)

-- Package.
import Trisagion.Parsers.Streamable (InputError (..))


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parsers.Word8 tests" $ do
    spec_word8
    spec_int8
    spec_word32Le
    spec_word32Be


-- Tests.
spec_word8 :: Spec
spec_word8 = describe "word8 tests" $ do
    it "Success case" $ do
        testSuccess
            word8
            [0, 1, 2, 3]
            0
            1

    it "Failure on end of input" $ do
        testError
            word8
            []
            (InputError 1)
            0

spec_int8 :: Spec
spec_int8 = describe "int8 tests" $ do
    it "Success case" $ do
        testSuccess
            int8
            [0, 1, 2, 3] 
            0
            1

    it "Success case with fromIntegral" $ do
        testSuccess
            int8
            [fromIntegral (-1 :: Int8), 1, 2, 3] 
            (-1)
            1

    it "Failure on end of input" $ do
        testError
            int8
            []
            (InputError 1)
            0

spec_word32Le :: Spec
spec_word32Le = describe "word32Le tests" $ do
    it "Success cases" $ do
       testSuccess
            word32Le
            [0, 0, 0, 0, 0, 0, 0, 0] 
            0
            4
       testSuccess
            word32Le
            [1, 0, 0, 0, 0, 0, 0, 0] 
            1
            4

       testSuccess
            word32Le
            [0, 1, 0, 0, 0, 0, 0, 0] 
            256
            4

       testSuccess
            word32Le
            [0, 0, 1, 0, 0, 0, 0, 0] 
            65536
            4

       testSuccess
            word32Le
            [0, 0, 0, 1, 0, 0, 0, 0] 
            16777216
            4

    it "Failure on end of input" $ do
        testError
            word32Le
            [0, 0, 0]
            (InputError 4)
            0

spec_word32Be :: Spec
spec_word32Be = describe "word32Be tests" $ do
    it "Success cases" $ do
       testSuccess
            word32Be
            [0, 0, 0, 0, 0, 0, 0, 0] 
            0
            4

       testSuccess
            word32Be
            [0, 0, 0, 1, 0, 0, 0, 0] 
            1
            4

       testSuccess
            word32Be
            [0, 0, 1, 0, 0, 0, 0, 0] 
            256
            4

       testSuccess
            word32Be
            [0, 1, 0, 0, 0, 0, 0, 0] 
            65536
            4

       testSuccess
            word32Be
            [1, 0, 0, 0, 0, 0, 0, 0] 
            16777216
            4

    it "Failure on end of input" $ do
        testError
            word32Be
            [0, 0, 0]
            (InputError 4)
            0
