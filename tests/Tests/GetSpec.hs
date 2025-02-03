module Tests.GetSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Lib.Helpers

-- Module to test.
import Trisagion.Get
import qualified Trisagion.Get as Get (maybe, zip, zipWith, either, sequence, repeat)

-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty (..))

-- Package.
import Trisagion.Types.ParseError (makeParseErrorNoBacktrace)
import Trisagion.Getters.Streamable (InputError (..), MatchError (..), matchElem, one, satisfy)
import Trisagion.Getters.ParseError (ValidationError (..))
import Trisagion.Streams.Counter (initialize)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Get tests" $ do
    spec_observe
    spec_lookAhead
    spec_maybe
    spec_zip
    spec_zipWith
    spec_between
    spec_either
    spec_sequence
    spec_repeat
    spec_many
    spec_atMostN
    spec_some
    spec_untilEnd
    spec_sepBy1


-- Tests.
spec_observe :: Spec
spec_observe = describe "observe tests" $ do
    it "Failure case" $ do
        testGetSuccess
            (observe (matchElem '1'))
            "0123"
            (Left $ makeParseErrorNoBacktrace (initialize "0123") (Right $ MatchError '1'))
            0

    it "End of input case" $ do
        testGetSuccess
            (observe one)
            ""
            (Left $ makeParseErrorNoBacktrace (initialize "") (InputError 1))
            0

spec_lookAhead :: Spec
spec_lookAhead = describe "lookAhead tests" $ do
    it "Success case" $ do
        testGetSuccess
            (lookAhead one)
            "0123"
            (Right '0')
            0

    it "Case of parser failure" $ do
        testGetSuccess
            (lookAhead (matchElem '1'))
            "0123"
            (Left $ makeParseErrorNoBacktrace (initialize "0123") (Right $ MatchError '1'))
            0

    it "End of input case" $ do
        testGetSuccess
            (lookAhead one)
            ""
            (Left $ makeParseErrorNoBacktrace (initialize "") (InputError 1))
            0

spec_maybe :: Spec
spec_maybe = describe "maybe tests" $ do
    it "Success case" $ do
        testGetSuccess
            (Get.maybe $ matchElem '0')
            "0123"
            (Just '0')
            1
    it "Failure case" $ do
        testGetSuccess
            (Get.maybe $ matchElem '1')
            "0123"
            Nothing
            0

    it "End of input case" $ do
        testGetSuccess
            (Get.maybe $ matchElem '0')
            ""
            Nothing
            0

spec_zip :: Spec
spec_zip = describe "zip tests" $ do
    it "Success case" $ do
        testGetSuccess
            (Get.zip one one)
            "0123"
            ('0','1')
            2

    it "Case of first parser failure" $ do
         testGetError
            (Get.zip (matchElem '1') (matchElem '1'))
            "0123"
            (Right $ MatchError '1')
            0

    it "Case of second parser failure" $ do
         testGetError
            (Get.zip (matchElem '0') (matchElem '2'))
            "0123"
            (Right $ MatchError '2')
            -- One character consumed.
            1

spec_zipWith :: Spec
spec_zipWith = describe "zipWith tests" $ do
    it "Success case" $ do
        testGetSuccess
            (Get.zipWith max one one)
            "0123"
            '1'
            2

spec_between :: Spec
spec_between = describe "between tests" $ do
    it "Success case" $ do
        testGetSuccess
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{1}3"
            '1'
            3

    it "Failure of opening parser" $ do
        testGetError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "11}3"
            (Right $ MatchError '{')
            0

    it "Failure of closing parser" $ do
        testGetError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{133"
            (Right $ MatchError '}')
            -- Two characters consumed.
            2

    it "Failure of opening parser on end of input" $ do
        testGetError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            ""
            (Left $ InputError 1)
            0

    it "Failure of between parser on end of input" $ do
        testGetError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{"
            (Left $ InputError 1)
            1

    it "Failure of closing parser on end of input" $ do
        testGetError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{1"
            (Left $ InputError 1)
            2

spec_either :: Spec
spec_either = describe "either tests" $ do
    it "Success of first parser" $ do
        testGetSuccess
            (Get.either one one)
            "0123"
            (Left '0')
            1

    it "Success on second parser" $ do
        testGetSuccess
            (Get.either (matchElem '1') (first (fmap Left) one))
            "0123"
            (Right '0')
            1

    it "Failure of both parsers" $ do
        testGetError
            (Get.either (matchElem '1') (matchElem '2'))
            "0123"
            (Right $ MatchError '1')
            0

    it "Failure of both parsers but with different errors" $ do
        testGetError
            (Get.either (matchElem '1') (bimap (fmap Left) snd $ Get.zip one one))
            "0"
            (Right $ MatchError '1')
            0

spec_sequence :: Spec
spec_sequence = describe "sequence tests" $ do
    it "Case of all parsers succeeding" $ do
        testGetSuccess
            (Get.sequence [matchElem '{', first (fmap Left) one, matchElem '}'])
            "{1}3"
            "{1}"
            3

spec_repeat :: Spec
spec_repeat = describe "repeat tests" $ do
    it "Success case" $ do
        testGetSuccess
            (Get.repeat 2 one)
            "0123"
            "01"
            2
    it "Failure case of insufficient input" $ do
        testGetError
            (Get.repeat 10 one)
            "0123"
            (InputError 1)
            4

    it "Failure when middle parser fails" $ do
        testGetError
            (Get.repeat 2 (matchElem '0'))
            "0123"
            (Right $ MatchError '0')
            1

spec_many :: Spec
spec_many = describe "many tests" $ do
    it "Success case" $ do
        testGetSuccess
            (many $ satisfy ('}' /=))
            "01}3"
            "01"
            2

    it "Case of first parser failure" $ do
        testGetSuccess
            (many $ satisfy ('0' /=))
            "0123"
            ""
            0

    it "Case of end of input" $ do
        testGetSuccess
            (many $ satisfy ('0' /=))
            ""
            ""
            0

spec_atMostN :: Spec
spec_atMostN = describe "atMostN tests" $ do
    it "Success case" $ do
        testGetSuccess
            (atMostN 2 one)
            "0123"
            "01"
            2

    it "Success on insufficient input" $ do
        testGetSuccess
            (atMostN 10 one)
            "0123"
            "0123"
            4

    it "Success even on parser failure" $ do
        testGetSuccess
            (atMostN 10 (matchElem '0'))
            "0123"
            "0"
            1

spec_some :: Spec
spec_some = describe "some tests" $ do
    it "Success case" $ do
        testGetSuccess
            (some $ satisfy ('}' /=))
            "01}3"
            ('0' :| "1")
            2

    it "Case when first parser fails" $ do
        testGetError
            (some $ satisfy ('0' /=))
            "0123"
            (Right ValidationError)
            0

spec_untilEnd :: Spec
spec_untilEnd = describe "untilEnd tests" $ do
    it "Success case" $ do
        testGetSuccess
            (untilEnd (matchElem '}') (first (fmap Left) one))
            "01}3"
            ('0' :| "1}")
            3

spec_sepBy1 :: Spec
spec_sepBy1 = describe "sepBy1 tests" $ do
    it "Success cases" $ do
        testGetSuccess
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            "0,1,2,321"
            ('0' :| "123")
            7

        testGetSuccess
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            "0123"
            ('0' :| "")
            1

    it "Case of end of input" $ do
        testGetError
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            ""
            (Left (InputError 1))
            0
