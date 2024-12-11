module Tests.Examples.Table.ParsersSpec (
    -- * Tests.
    spec,
) where

-- Imports.
-- Testing library.
import Test.Hspec

-- Testing helpers.
import Tests.Helpers

-- Module to test.
import Trisagion.Examples.Table.Parsers

-- Auxiliary imports.
-- Base.
import Data.List.NonEmpty ((<|), singleton)

-- Libraries.
import Data.Text qualified as Text (pack, empty)

-- Package.
import Trisagion.Getters.Streamable (MatchError (..), InputError (..))


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Examples.Table tests" $ do
    spec_parseHeader
    spec_parseComment
    spec_parseFieldOrComment
    spec_parseLine
    spec_parseFields


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

    it "Failure on no input" $ do
        testError
            parseComment
            Text.empty
            Text.empty
            (Left $ InputError 1)

    it "Failure on incorrect comment starting character" $ do
        testError
            parseComment
            (Text.pack "/comment with wrong character")
            (Text.pack "/comment with wrong character")
            (Right $ MatchError '#')

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

spec_parseLine :: Spec
spec_parseLine = describe "parseLine" $ do
    it "Success in case of only whitespace line" $ do
        testTableSuccess
            parseLine
            "    "
            []
            (1, "")

    it "Success in case of comment line" $ do
        testTableSuccess
            parseLine
            "#a commentary"
            []
            (1, "")

        testTableSuccess
            parseLine
            "     #a commentary"
            []
            (1, "")

    it "Success case for fields" $ do
        testTableSuccess
            parseLine
            "    some name"
            (Text.pack <$> ["some", "name"])
            (1, "")

        testTableSuccess
            parseLine
            "    some name #some commentary"
            (Text.pack <$> ["some", "name"])
            (1, "")

spec_parseFields :: Spec
spec_parseFields = describe "parseFields" $ do
    it "Success case" $ do
        testTableSuccess
            parseFields
            "some arbitrary text"
            (Text.pack <$> ("some" <| "arbitrary" <| singleton "text"))
            (1, "")

    it "Success with leading whitespace" $ do
        testTableSuccess
            parseFields
            " \r\v\t\f   some arbitrary text"
            (Text.pack <$> ("some" <| "arbitrary" <| singleton "text"))
            (1, "")

    it "Success with comment character" $ do
        testTableSuccess
            parseFields
            "some #arbitrary omment"
            (Text.pack <$> singleton "some" )
            (1, "")

        testTableSuccess
            parseFields
            "some arbi##trary text"
            (Text.pack <$> ("some" <| "arbi##trary" <| singleton "text"))
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
            (Right FieldsError)
            (0, "    #some arbitrary comment")
