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

-- Libraries.
import qualified Data.Text as Text (pack, empty)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Examples.Table tests" $ do
    spec_parseHeader
    spec_parseComment


-- Tests.
spec_parseHeader :: Spec
spec_parseHeader = describe "parseHeader" $ do
    it "Success case" $ do
        testTableSuccess
            parseHeader
            "tbl-v1.0\nsome more text"
            (Text.pack "tbl-v1.0")
            (1, "some more text")

    it "Failure on mismatch" $ do
        testTableError
            parseHeader
            "some arbitrary text\nsome more text"
            SignatureError
            (0, "some arbitrary text")

    it "Failure on leading whitespace" $ do
        testTableError
            parseHeader
            "   tbl-v1.0\nsome more text"
            SignatureError
            (0, "   tbl-v1.0")

    it "Failure on wrong casing" $ do
        testTableError
            parseHeader
            "TBL-v1.0\nsome more text"
            SignatureError
            (0, "TBL-v1.0")

spec_parseComment :: Spec
spec_parseComment = describe "parseComment" $ do
    it "Success case" $ do
        testSuccess
            parseComment
            (Text.pack "#some arbitrary text")
            (Text.pack "some arbitrary text")
            Text.empty
