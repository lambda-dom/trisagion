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

-- Base.
import Data.List.NonEmpty (NonEmpty (..))

-- Package.
import Trisagion.Getters.Streamable (InputError (..), MatchError (..) )


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Examples.Table tests" $ do
    spec_parseHeader
    spec_parseComment
    spec_parseFieldOrComment
    spec_parseRow
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

spec_parseFieldOrComment :: Spec
spec_parseFieldOrComment = describe "parseFieldOrComment" $ do
    it "Success on comment" $ do
        testSuccess
            parseFieldOrComment
            (Text.pack "#some arbitrary text")
            (Left $ Text.pack "some arbitrary text")
            Text.empty

    it "Success on field" $ do
        testSuccess
            parseFieldOrComment
            (Text.pack "some #arbitrary text")
            (Right $ Text.pack "some")
            (Text.pack " #arbitrary text")

    it "Success on whitespace" $ do
        testSuccess
            parseFieldOrComment
            (Text.pack "    ")
            (Right Text.empty)
            (Text.pack "    ")

    it "No input" $ do
        testSuccess
            parseFieldOrComment
            Text.empty
            (Right Text.empty)
            Text.empty

spec_parseRow :: Spec
spec_parseRow = describe "parseRow" $ do
    it "Success in case of only whitespace line" $ do
        testTableSuccess
            parseRow
            "    "
            []
            (1, "")

    it "Success in case of comment line" $ do
        testTableSuccess
            parseRow
            "#a commentary"
            []
            (1, "")

        testTableSuccess
            parseRow
            "     #a commentary"
            []
            (1, "")

    it "Success case for fields" $ do
        testTableSuccess
            parseRow
            "    some name"
            (Text.pack <$> ["some", "name"])
            (1, "")

        testTableSuccess
            parseRow
            "    some name #some commentary"
            (Text.pack <$> ["some", "name"])
            (1, "")

spec_parseFields :: Spec
spec_parseFields = describe "parseFields" $ do
    it "Success case" $ do
        testTableSuccess
            parseFields
            "some arbitrary text"
            (Text.pack <$> ("some" :| ["arbitrary", "text"]))
            (1, "")

    it "Success with leading whitespace" $ do
        testTableSuccess
            parseFields
            " \r\v\t\f   some arbitrary text"
            (Text.pack <$> ("some" :| ["arbitrary", "text"]))
            (1, "")

    it "Success with comment character" $ do
        testTableSuccess
            parseFields
            "some #arbitrary omment"
            (Text.pack <$> ("some" :| []))
            (1, "")

        testTableSuccess
            parseFields
            "some arbi##trary text"
            (Text.pack <$> ("some" :| ["arbi##trary", "text"]))
            (1, "")

    it "Failure on no input" $ do
        testTableError
            parseFields
            ""
            (Left $ InputError 1)
            (0, "")

    it "Failure on empty list of fields" $ do
        testTableError
            parseFields
            "    #some arbitrary comment"
            (Right EmptyLineError)
            (0, "    #some arbitrary comment")
