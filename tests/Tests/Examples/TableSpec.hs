module Tests.Examples.TableSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Tests.Helpers

-- Module to test.
import Trisagion.Examples.Table

-- Auxiliary imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))

-- Libraries.
import qualified Data.Text as Text (pack)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Examples.Table tests" $ do
    spec_parseTable


-- Tests.
spec_parseTable :: Spec
spec_parseTable = describe "parseTable" $ do
    it "Failure on no input" $ do
        testTableError
            (parseTable id)
            ""
            HeaderError
            (0, "")

    it "Failure on incorrect signature" $ do
        testTableError
            (parseTable id)
            "incorrect signature"
            HeaderError
            (0, "incorrect signature")

    it "Success with empty list of rows" $ do
        testTableSuccess
            (parseTable id)
            "tbl-v1.0\nsome arbitrary field"
            (makeTable [])
            (2, "")

    it "Success case" $ do
        testTableSuccess
            (parseTable toList)
            "tbl-v1.0\nsome arbitrary field\n1 2 3\n2 1 4"
            (makeTable [
                bimap Text.pack Text.pack <$> [("some","1"), ("arbitrary","2"), ("field","3")],
                bimap Text.pack Text.pack <$> [("some","2"), ("arbitrary","1"), ("field","4")]
            ])
            (4, "")
