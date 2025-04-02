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

-- Base.
import Control.Applicative (Alternative (..))
import Data.Void (Void)

-- Package.
import Trisagion.Parser (ParserPE)
import Trisagion.Parsers.ParseError (throwParseError)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Parser tests" $ do
    spec_alternative


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
