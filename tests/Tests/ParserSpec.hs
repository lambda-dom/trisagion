module Tests.ParserSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Testing helpers.
import Lib.Helpers (testError, testFail)

-- Module to test.
import Trisagion.Parser

-- Test imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Void (Void)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parsers.ParseError (throwParseError)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parser tests" $ do
    spec_alternative


spec_alternative :: Spec
spec_alternative = describe "Alternative tests" $ do
    it "Empty case" $ do
        let p = empty :: Parser s (ParseError ()) Void
        testFail
            p
            ""

    it "Left identity test case" $ do
        let p = empty :: Parser s (ParseError ()) Void
        testError
            (p <|> throwParseError ())
            ""
            ()
            0
