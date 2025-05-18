module Tests.ParserSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it)

-- Testing helpers.
import Lib.Helpers

-- Module to test.
import Trisagion.Parser

-- Test imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Void (Void)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Data.Bifunctor (Bifunctor(..))


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

    let f = bimap (const unitError) (const id) $ matchOne '0'
    let g = first (const unitError) (matchOne '0')
          *> bimap (const unitError) (const id) (matchOne '1')
    let x = first (const unitError) (matchOne '2')

    it "Failure of right distributivity I: (f <|> g) <*> x fails." $ do
        testFail
            ((f <|> g) <*> x)
            "0123"

    it "Failure of right distributivity II: (f <*> x) <|> (g <*> x) succeeds." $ do
        testSuccess
            ((f <*> x) <|> (g <*> x))
            "0123"
            '2'
            3
