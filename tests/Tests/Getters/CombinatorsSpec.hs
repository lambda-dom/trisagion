module Tests.Getters.CombinatorsSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Tests.Helpers

-- Module to test.
import Trisagion.Getters.Combinators
import Trisagion.Getters.Combinators qualified as Getters
    (either, maybe, repeat, sequence, until, zip, zipWith)

-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty (..))

-- Package.
import Trisagion.Types.ParseError (makeParseError)
import Trisagion.Getters.Streamable


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Getters.Combinators tests" $ do
    spec_observe
    spec_lookAhead
    spec_maybe
    spec_validate
    spec_failIff
    spec_zip
    spec_zipWith
    spec_between
    spec_either
    spec_sequence
    spec_repeat
    spec_many
    spec_some
    spec_atMostN
    spec_until
    spec_untilEnd
    spec_sepBy1


-- Tests.
spec_observe :: Spec
spec_observe = describe "observe" $ do
    it "Success case" $ do
        testSuccess
            (observe one)
            "0123"
            (Right '0')
            "123"

    it "Failure case" $ do
        testSuccess
            (observe (matchElem '1'))
            "0123"
            (Left $ makeParseError "0123" (Right $ MatchError '1'))
            "0123"

    it "End of input case" $ do
        testSuccess
            (observe one)
            ""
            (Left $ makeParseError "" (InputError 1))
            ""

spec_lookAhead :: Spec
spec_lookAhead = describe "lookAhead" $ do
    it "Success case" $ do
        testSuccess
            (lookAhead one)
            "0123"
            (Right '0')
            "0123"

    it "Case of parser failure" $ do
        testSuccess
            (lookAhead (matchElem '1'))
            "0123"
            (Left $ makeParseError "0123" (Right $ MatchError '1'))
            "0123"

    it "End of input case" $ do
        testSuccess
            (lookAhead one)
            ""
            (Left $ makeParseError "" (InputError 1))
            ""

spec_maybe :: Spec
spec_maybe = describe "maybe" $ do
    it "Success case" $ do
        testSuccess
            (Getters.maybe $ matchElem '0')
            "0123"
            (Just '0')
            "123"

    it "Failure case" $ do
        testSuccess
            (Getters.maybe $ matchElem '1')
            "0123"
            Nothing
            "0123"

    it "End of input case" $ do
        testSuccess
            (Getters.maybe $ matchElem '0')
            ""
            Nothing
            ""

spec_validate :: Spec
spec_validate = describe "validate" $ do
    it "Success case" $ do
        testSuccess
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            "0123"
            '0'
            "123"

    it "Case of parser failure" $ do
         testError
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            ""
            ""
            (Left $ InputError 1)

    it "Case of parser failure -- parser state advanced" $ do
         testError
            (validate
                (\ c -> if c == '0' then Right c else Left ())
                (first (fmap Left) one *> matchElem '1'))
            "023"
            "23"
            (Left $ Right $ MatchError '1')

    it "Case of failed validation -- parser state not advanced" $ do
         testError
            (validate (\ c -> if c == '0' then Right c else Left ()) one)
            "123"
            "123"
            (Right $ ())

spec_failIff :: Spec
spec_failIff = describe "failIff" $ do
    it "Success case" $ do
        testSuccess
            (failIff $ matchElem '1')
            "0123"
            ()
            "0123"

    it "Failure case" $ do
        testFail
            (failIff $ matchElem '0')
            "0123"

    it "End of input case" $ do
        testSuccess
            (failIff $ matchElem '0')
            ""
            ()
            ""

spec_zip :: Spec
spec_zip = describe "zip" $ do
    it "Success case" $ do
        testSuccess
            (Getters.zip one one)
            "0123"
            ('0','1')
            "23"

    it "Case of first parser failure" $ do
         testError
            (Getters.zip (matchElem '1') (matchElem '1'))
            "0123"
            "0123"
            (Right $ MatchError '1')

    it "Case of second parser failure" $ do
         testError
            (Getters.zip (matchElem '0') (matchElem '2'))
            "0123"
            -- One character consumed.
            "123"
            (Right $ MatchError '2')

spec_zipWith :: Spec
spec_zipWith = describe "zipWith" $ do
    it "Success case" $ do
        testSuccess
            (Getters.zipWith max one one)
            "0123"
            '1'
            "23"

spec_between :: Spec
spec_between = describe "between" $ do
    it "Success case" $ do
        testSuccess
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{1}3"
            '1'
            "3"

    it "Failure of opening parser" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "11}3"
            "11}3"
            (Right $ MatchError '{')

    it "Failure of closing parser" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{133"
            -- Two characters consumed.
            "33"
            (Right $ MatchError '}')

    it "Failure of opening parser on end of input" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            ""
            ""
            (Left $ InputError 1)

    it "Failure of between parser on end of input" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{"
            ""
            (Left $ InputError 1)

    it "Failure of closing parser on end of input" $ do
        testError
            (between (matchElem '{') (matchElem '}') (first (fmap Left) one))
            "{1"
            ""
            (Left $ InputError 1)

spec_either :: Spec
spec_either = describe "either" $ do
    it "Success of first parser" $ do
        testSuccess
            (Getters.either one one)
            "0123"
            (Left '0')
            "123"

    it "Success on second parser" $ do
        testSuccess
            (Getters.either (matchElem '1') (first (fmap Left) one))
            "0123"
            (Right '0')
            "123"

    it "Failure of both parsers" $ do
        testError
            (Getters.either (matchElem '1') (matchElem '2'))
            "0123"
            "0123"
            (Right $ MatchError '1')

    it "Failure of both parsers but with different errors" $ do
        testError
            (Getters.either (matchElem '1') (bimap (fmap Left) snd $ Getters.zip one one))
            "0"
            "0"
            (Right $ MatchError '1')

spec_sequence :: Spec
spec_sequence = describe "sequence" $ do
    it "Case of all parsers succeeding" $ do
        testSuccess
            (Getters.sequence [matchElem '{', first (fmap Left) one, matchElem '}'])
            "{1}3"
            "{1}"
            "3"

spec_repeat :: Spec
spec_repeat = describe "repeat" $ do
    it "Success case" $ do
        testSuccess
            (Getters.repeat 2 one)
            "0123"
            "01"
            "23"

    it "Failure case of insufficient input" $ do
        testError
            (Getters.repeat 10 one)
            "0123"
            ""
            (InputError 1)

    it "Failure when middle parser fails" $ do
        testError
            (Getters.repeat 2 (matchElem '0'))
            "0123"
            "123"
            (Right $ MatchError '0')

spec_many :: Spec
spec_many = describe "many" $ do
    it "Success case" $ do
        testSuccess
            (many (satisfy ('}' /=)))
            "01}3"
            "01"
            "}3"

    it "Case of first parser failure" $ do
        testSuccess
            (many (satisfy ('0' /=)))
            "0123"
            ""
            "0123"

    it "Case of end of input" $ do
        testSuccess
            (many (satisfy ('0' /=)))
            ""
            ""
            ""

spec_atMostN :: Spec
spec_atMostN = describe "atMostN" $ do
    it "Success case" $ do
        testSuccess
            (atMostN 2 one)
            "0123"
            "01"
            "23"

    it "Success on insufficient input" $ do
        testSuccess
            (atMostN 10 one)
            "0123"
            "0123"
            ""

    it "Success even on parser failure" $ do
        testSuccess
            (atMostN 10 (matchElem '0'))
            "0123"
            "0"
            "123"

spec_some :: Spec
spec_some = describe "some" $ do
    it "Success case" $ do
        testSuccess
            (some (satisfy ('}' /=)))
            "01}3"
            ('0' :| "1")
            "}3"

    it "Case when first parser fails" $ do
        testError
            (some (satisfy ('0' /=)))
            "0123"
            "0123"
            (Right ValidationError)

spec_until :: Spec
spec_until = describe "until" $ do
    it "Success case" $ do
        testSuccess
            (Getters.until (matchElem '}') (first (fmap Left) one))
            "01}3"
            "01"
            "}3"

    it "Case of failure of first parser" $ do
        testSuccess
            (Getters.until (matchElem '}') (first (fmap Left) one))
            "}012"
            ""
            "}012"

spec_untilEnd :: Spec
spec_untilEnd = describe "untilEnd" $ do
    it "Success case" $ do
        testSuccess
            (untilEnd (matchElem '}') (first (fmap Left) one))
            "01}3"
            ('0' :| "1}")
            "3"

spec_sepBy1 :: Spec
spec_sepBy1 = describe "sepBy1" $ do
    it "Success cases" $ do
        testSuccess
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            "0,1,2,321"
            ('0' :| "123")
            "21"

        testSuccess
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            "0123"
            ('0' :| "")
            "123"

    it "Case of end of input" $ do
        testError
            (sepBy1 (matchElem ',') (first (fmap Left) one))
            ""
            ""
            (Left (InputError 1))
