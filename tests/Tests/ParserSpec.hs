{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, left identity" #-}

module Tests.ParserSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Test runner helpers.
import Lib.Runners

-- Module to test.
import Trisagion.Parser (ParserPE, InputError (..), backtrack, one, takePrefix, takePrefixWith)

-- Base.
import Control.Applicative (Alternative (..))
import Data.Void (Void)

-- Package.
import Trisagion.Parsers.ParseError (ValidationError (..), throwParseError)
import Trisagion.Streams.Counter (initialize)
import Trisagion.Types.ParseError (makeParseError)
import Trisagion.Parsers.Streamable (matchElem)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parser tests" $ do
    spec_alternative
    spec_one
    spec_backtrack
    spec_takePrefix
    spec_takePrefixWith


-- Tests.
spec_alternative :: Spec
spec_alternative = describe "Alternative tests" $ do
    it "Empty case" $ do
        let p = empty :: ParserPE s Void Void
        testFail
            p
            ""

    it "Left identity" $ do
        let p = empty :: ParserPE s () Void
        testError
            (p <|> throwParseError ())
            ""
            ()
            0

spec_one :: Spec
spec_one = describe "one tests" $ do
    it "Success case" $ do
        testSuccess
            one
            "0123" 
            '0'
            1

    it "Failure on end of input" $ do
        testError
            one
            ""
            (InputError 1)
            0

spec_backtrack :: Spec
spec_backtrack = describe "backtrack tests" $ do
    it "End of input case" $ do
        testSuccess
            (backtrack one)
            ""
            (Left $ makeParseError (initialize "") (InputError 1))
            0

    it "Failure case" $ do
        testSuccess
            (backtrack (matchElem '1'))
            "0123"
            (Left $ makeParseError (initialize "0123") (Right $ ValidationError '0'))
            0

spec_takePrefix :: Spec
spec_takePrefix = describe "takePrefix tests" $ do
    it "Success case" $ do
        testSuccess
            (takePrefix 2)
            "0123"
            "01"
            2

    it "Case of insufficient input" $ do
        testSuccess
            (takePrefix 10)
            "0123"
            "0123"
            4

spec_takePrefixWith :: Spec
spec_takePrefixWith = describe "takePrefixWith tests" $ do
    it "Success case" $ do
        testSuccess
            (takePrefixWith ('3' /=))
            "0123"
            "012"
            3

    it "No input case" $ do
        testSuccess
            (takePrefixWith ('3' /=))
            ""
            ""
            0
