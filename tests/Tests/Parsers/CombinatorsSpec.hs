module Tests.Parsers.CombinatorsSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Test runner helpers.
import Lib.Runners

-- Module to test.
import Trisagion.Parsers.Combinators (lookAhead, between, many, some, untilEnd, sepBy1, atMostN)
import qualified Trisagion.Parsers.Combinators as Parsers (maybe, zip, zipWith, either, sequence, repeat)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
-- import qualified Control.Applicative as Base (many)

-- Package.
import Trisagion.Types.ParseError (makeParseError)
import Trisagion.Streams.Counter (initialize)
import Trisagion.Parser (InputError (..), one)
import Trisagion.Parsers.ParseError (ValidationError (..))
import Trisagion.Parsers.Streamable (matchElem, satisfy)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parsers.Combinators tests" $ do
    spec_lookAhead
    spec_maybe
    spec_zip
    spec_zipWith
    spec_between
    spec_either
    spec_sequence
    spec_repeat
    spec_many
    spec_some
    spec_untilEnd
    spec_sepBy1
    spec_atMostN


-- Tests.
spec_lookAhead :: Spec
spec_lookAhead = describe "lookAhead tests" $ do
    it "Success case" $ do
        testSuccess
            (lookAhead one)
            "0123"
            (Right '0')
            0

    it "Case of parser failure" $ do
        testSuccess
            (lookAhead (matchElem '1'))
            "0123"
            (Left $ makeParseError (initialize "0123") (Right $ ValidationError '0'))
            0

    it "End of input case" $ do
        testSuccess
            (lookAhead one)
            ""
            (Left $ makeParseError (initialize "") (InputError 1))
            0

spec_maybe :: Spec
spec_maybe = describe "maybe tests" $ do
    it "Success case" $ do
        testSuccess
            (Parsers.maybe $ matchElem '0')
            "0123"
            (Just '0')
            1

    it "Failure case" $ do
        testSuccess
            (Parsers.maybe $ matchElem '1')
            "0123"
            Nothing
            0

    it "End of input case" $ do
        testSuccess
            (Parsers.maybe $ matchElem '0')
            ""
            Nothing
            0

spec_zip :: Spec
spec_zip = describe "zip tests" $ do
    it "Success case" $ do
        testSuccess
            (Parsers.zip one one)
            "0123"
            ('0','1')
            2

    it "Case of first parser failure" $ do
         testError
            (Parsers.zip (matchElem '1') (matchElem '1'))
            "0123"
            (Right $ ValidationError '0')
            0

    it "Case of second parser failure" $ do
         testError
            (Parsers.zip (matchElem '0') (matchElem '2'))
            "0123"
            (Right $ ValidationError '1')
            -- One character consumed.
            1

spec_zipWith :: Spec
spec_zipWith = describe "zipWith tests" $ do
    it "Success case" $ do
        testSuccess
            (Parsers.zipWith max one one)
            "0123"
            '1'
            2

spec_between :: Spec
spec_between = describe "between tests" $ do
    it "Success case" $ do
        testSuccess
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{1}3"
            '1'
            3

    it "Failure of opening parser" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "11}3"
            (Right $ ValidationError '1')
            0

    it "Failure of closing parser" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{133"
            (Right $ ValidationError '3')
            -- Two characters consumed.
            2

    it "Failure of opening parser on end of input" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            ""
            (Left $ InputError 1)
            0

    it "Failure of between parser on end of input" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{"
            (Left $ InputError 1)
            1

    it "Failure of closing parser on end of input" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{1"
            (Left $ InputError 1)
            2

spec_either :: Spec
spec_either = describe "either tests" $ do
    it "Success of first parser" $ do
        testSuccess
            (Parsers.either one one)
            "0123"
            (Left '0')
            1

    it "Success on second parser" $ do
        testSuccess
            (Parsers.either (matchElem '1') (first (fmap Left) one))
            "0123"
            (Right '0')
            1

    it "Failure of both parsers" $ do
        testError
            (Parsers.either (matchElem '1') (matchElem '2'))
            "0123"
            (Right $ ValidationError '0')
            0

    it "Failure of both parsers but with different errors" $ do
        testError
            (Parsers.either (matchElem '1') (bimap (fmap Left) snd $ Parsers.zip one one))
            "0"
            (Right $ ValidationError '0')
            0

spec_sequence :: Spec
spec_sequence = describe "sequence tests" $ do
    it "Case of all parsers succeeding" $ do
        testSuccess
            (Parsers.sequence [matchElem '{', first (fmap Left) one, matchElem '}'])
            "{1}3"
            "{1}"
            3

spec_repeat :: Spec
spec_repeat = describe "repeat tests" $ do
    it "Success case" $ do
        testSuccess
            (Parsers.repeat 2 one)
            "0123"
            "01"
            2

    it "Failure case of insufficient input" $ do
        testError
            (Parsers.repeat 10 one)
            "0123"
            (InputError 1)
            4

    it "Failure when parser after first fails" $ do
        testError
            (Parsers.repeat 2 (matchElem '0'))
            "0123"
            (Right $ ValidationError '1')
            1

-- spec_manyAlternative :: Spec
-- spec_manyAlternative = describe "tests for lazyness of many from Alternative" $ do
--     let p = take 2 <$> Base.many one
--     it "result and position" $ do
--         testSuccess
--             p
--             "0123"
--             "01"
--             2

--     it "remainder" $ do
--         let r = remainder p (initialize "0123")
--         fmap stream r `shouldBe` Right "23"

spec_many :: Spec
spec_many = describe "many tests" $ do
    it "Success case" $ do
        testSuccess
            (many $ satisfy ('}' /=))
            "01}3"
            "01"
            2

    it "Case of parser failure on head of input" $ do
        testSuccess
            (many $ satisfy ('0' /=))
            "0123"
            ""
            0

    it "Case of end of input" $ do
        testSuccess
            (many $ satisfy ('0' /=))
            ""
            ""
            0

    it "'take n <$> many' consumes *all* input" $ do
        testSuccess
            (take 2 <$> many one)
            "0123"
            "01"
            4

spec_some :: Spec
spec_some = describe "some tests" $ do
    it "Success case" $ do
        testSuccess
            (some $ satisfy ('}' /=))
            "01}3"
            ('0' :| "1")
            2

    it "Case when parser fails on head of input" $ do
        testError
            (some $ satisfy ('0' /=))
            "0123"
            (Right $ ValidationError '0')
            0

spec_untilEnd :: Spec
spec_untilEnd = describe "untilEnd tests" $ do
    it "Success case" $ do
        testSuccess
            (untilEnd (matchElem '}') (first (fmap Left) one))
            "01}3"
            ('0' :| "1}")
            3

spec_sepBy1 :: Spec
spec_sepBy1 = describe "sepBy1 tests" $ do
    it "Success cases" $ do
        testSuccess
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            "0,1,2,321"
            ('0' :| "123")
            7

        testSuccess
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            "0123"
            ('0' :| "")
            1

    it "Case of end of input" $ do
        testError
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            ""
            (Left (InputError 1))
            0

spec_atMostN :: Spec
spec_atMostN = describe "atMostN tests" $ do
    it "Success cases" $ do
        testSuccess
            (atMostN 2 one)
            "0123"
            "01"
            2

    it "Success on insufficient input" $ do
        testSuccess
            (atMostN 10 one)
            "0123"
            "0123"
            4

    it "case of parser failure" $ do
        testSuccess
            (atMostN 10 (matchElem '0'))
            "0123"
            "0"
            1
