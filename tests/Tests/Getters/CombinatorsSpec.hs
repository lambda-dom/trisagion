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

-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty (..))

-- Package.
import Trisagion.Types.ParseError (makeParseError)
import Trisagion.Getters.Streamable


-- Main module test driver.
spec :: Spec
spec = describe "Bins.Getters.Combinators tests" $ do
    spec_observe
    spec_lookAhead
    spec_option
    spec_failIff
    spec_pair
    spec_pairWith
    spec_between
    spec_eitherOf
    spec_chain
    spec_repeatN
    spec_repeatedly
    spec_atMostN
    spec_atLeastOne
    spec_manyTill
    spec_manyTill_
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

spec_option :: Spec
spec_option = describe "option" $ do
    it "Success case" $ do
        testSuccess
            (option $ matchElem '0')
            "0123"
            (Just '0')
            "123"

    it "Failure case" $ do
        testSuccess
            (option $ matchElem '1')
            "0123"
            Nothing
            "0123"

    it "End of input case" $ do
        testSuccess
            (option $ matchElem '0')
            ""
            Nothing
            ""

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

spec_pair :: Spec
spec_pair = describe "pair" $ do
    it "Success case" $ do
        testSuccess
            (pair one one)
            "0123"
            ('0','1')
            "23"

    it "Case of first parser failure" $ do
         testError
            (pair (matchElem '1') (matchElem '1'))
            "0123"
            "0123"
            (Right $ MatchError '1')

    it "Case of second parser failure" $ do
         testError
            (pair (matchElem '0') (matchElem '2'))
            "0123"
            -- One character consumed.
            "123"
            (Right $ MatchError '2')

spec_pairWith :: Spec
spec_pairWith = describe "pairWith" $ do
    it "Success case" $ do
        testSuccess
            (pairWith max one one)
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

spec_eitherOf :: Spec
spec_eitherOf = describe "eitherOf" $ do
    it "Success of first parser" $ do
        testSuccess
            (eitherOf one one)
            "0123"
            (Left '0')
            "123"

    it "Success on second parser" $ do
        testSuccess
            (eitherOf (matchElem '1') (first (fmap Left) one))
            "0123"
            (Right '0')
            "123"

    it "Failure of both parsers" $ do
        testError
            (eitherOf (matchElem '1') (matchElem '2'))
            "0123"
            "0123"
            (Right $ MatchError '1')

    it "Failure of both parsers but with different errors" $ do
        testError
            (eitherOf (matchElem '1') (bimap (fmap Left) snd $ pair one one))
            "0"
            "0"
            (Right $ MatchError '1')

spec_chain :: Spec
spec_chain = describe "chain" $ do
    it "Case of all parsers succeeding" $ do
        testSuccess
            (chain [matchElem '{', first (fmap Left) one, matchElem '}'])
            "{1}3"
            "{1}"
            "3"

spec_repeatN :: Spec
spec_repeatN = describe "repeatN" $ do
    it "Success case" $ do
        testSuccess
            (repeatN 2 one)
            "0123"
            "01"
            "23"

    it "Failure case of insufficient input" $ do
        testError
            (repeatN 10 one)
            "0123"
            ""
            (InputError 1)

    it "Failure when middle parser fails" $ do
        testError
            (repeatN 2 (matchElem '0'))
            "0123"
            "123"
            (Right $ MatchError '0')

spec_repeatedly :: Spec
spec_repeatedly = describe "repeatedly" $ do
    it "Success case" $ do
        testSuccess
            (repeatedly (satisfy ('}' /=)))
            "01}3"
            "01"
            "}3"

    it "Case of first parser failure" $ do
        testSuccess
            (repeatedly (satisfy ('0' /=)))
            "0123"
            ""
            "0123"

    it "Case of end of input" $ do
        testSuccess
            (repeatedly (satisfy ('0' /=)))
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

spec_atLeastOne :: Spec
spec_atLeastOne = describe "atLeastOne" $ do
    it "Success case" $ do
        testSuccess
            (atLeastOne (satisfy ('}' /=)))
            "01}3"
            ('0' :| "1")
            "}3"

    it "Case when first parser fails" $ do
        testError
            (atLeastOne (satisfy ('0' /=)))
            "0123"
            "0123"
            (Right ValidationError)

spec_manyTill :: Spec
spec_manyTill = describe "manyTill" $ do
    it "Success case" $ do
        testSuccess
            (manyTill (matchElem '}') (first (fmap Left) one))
            "01}3"
            "01"
            "}3"

    it "Case of failure of first parser" $ do
        testSuccess
            (manyTill (matchElem '}') (first (fmap Left) one))
            "}012"
            ""
            "}012"

spec_manyTill_ :: Spec
spec_manyTill_ = describe "manyTill_" $ do
    it "Success case" $ do
        testSuccess
            (manyTill_ (matchElem '}') (first (fmap Left) one))
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
