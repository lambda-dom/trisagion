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

-- Package.
import Trisagion.Getters.Streamable (InputError (..), MatchError (..) )


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Examples.Table tests" $ do
    spec_parseHeader
    spec_parseComment
    spec_parseFields


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
            (Right $ MatchError (Text.pack "tbl-v1.0"))
            (0, "some arbitrary text")

    it "Failure on leading whitespace" $ do
        testTableError
            parseHeader
            "   tbl-v1.0\nsome more text"
            (Right $ MatchError (Text.pack "tbl-v1.0"))
            (0, "   tbl-v1.0")

    it "Failure on wrong casing" $ do
        testTableError
            parseHeader
            "TBL-v1.0\nsome more text"
            (Right $ MatchError (Text.pack "tbl-v1.0"))
            (0, "TBL-v1.0")

spec_parseComment :: Spec
spec_parseComment = describe "parseComment" $ do
    it "Success case" $ do
        testSuccess
            parseComment
            (Text.pack "#some arbitrary text")
            (Text.pack "some arbitrary text")
            Text.empty

spec_parseFields :: Spec
spec_parseFields = describe "parseFields" $ do
    it "Success case" $ do
        testTableSuccess
            parseFields
            "some arbitrary text"
            (Text.pack <$> ["some", "arbitrary", "text"])
            (1, "")

    it "Success with leading whitespace" $ do
        testTableSuccess
            parseFields
            " \r\v\t\f   some arbitrary text"
            (Text.pack <$> ["some", "arbitrary", "text"])
            (1, "")

    it "Success with empty line" $ do
        testTableSuccess
            parseFields
            "    "
            []
            (1, "")

    it "Success with comment character" $ do
        testTableSuccess
            parseFields
            "some #arbitrary text"
            (Text.pack <$> ["some", "#arbitrary", "text"])
            (1, "")

        testTableSuccess
            parseFields
            "some arbi##trary text"
            (Text.pack <$> ["some", "arbi##trary", "text"])
            (1, "")

    it "Failure on no input" $ do
        testTableError
            parseFields
            ""
            (InputError 1)
            (0, "")
