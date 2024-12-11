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
import Data.List.NonEmpty ((<|), singleton)
import Data.Void (Void)

-- Libraries.
import qualified Data.Text as Text (pack)


-- Main module test driver.
spec :: Spec
spec = describe "Trisagion.Examples.Table tests" $ do
    spec_parseRows
    spec_parseTable


-- Tests.
spec_parseRows :: Spec
spec_parseRows = describe "parseRows" $ do
    it "Success case" $ do
        testTableSuccess
            (parseRows Right (Text.pack <$> ("some" <| singleton "field")))
            "1 2\n2 1"
            [
                bimap Text.pack Text.pack <$> (("some","1") <| singleton ("field","2")),
                bimap Text.pack Text.pack <$> (("some","2") <| singleton ("field","1"))
            ]
            (2, "")

    it "Success on no input" $ do
        testTableSuccess
            (parseRows Right (Text.pack <$> ("some" <| singleton "field")))
            ""
            []
            (0, "")
            
    it "Success with comment lines" $ do
        testTableSuccess
            (parseRows Right (Text.pack <$> ("some" <| singleton "field")))
            "1 2 #first comment\n2 1 #second comment"
            [
                bimap Text.pack Text.pack <$> (("some","1") <| singleton ("field","2")),
                bimap Text.pack Text.pack <$> (("some","2") <| singleton ("field","1"))
            ]
            (2, "")

    it "Failure on mismatched number of fields" $ do
        testTableError
            (parseRows Right (Text.pack <$> ("some" <| singleton "field")))
            "1 2\n2 1 spurious"
            (MismatchError :: TableError Void)
            (1, "2 1 spurious")

    it "Failure on validation" $ do
        testTableError
            (parseRows (const $ Left ()) (Text.pack <$> ("some" <| singleton "field")))
            "a b\n2 1"
            (RowError ())
            (0, "a b")

spec_parseTable :: Spec
spec_parseTable = describe "parseTable" $ do
    it "Failure on no input" $ do
        testTableError
            (parseTable Right)
            ""
            (HeaderError :: TableError Void)
            (0, "")

    it "Failure on incorrect signature" $ do
        testTableError
            (parseTable Right)
            "incorrect signature"
            (HeaderError :: TableError Void)
            (0, "incorrect signature")

    it "Success with empty list of rows" $ do
        testTableSuccess
            (parseTable Right)
            "tbl-v1.0\nsome arbitrary field"
            (makeTable [])
            (2, "")

    it "Success case" $ do
        testTableSuccess
            (parseTable (Right . toList))
            "tbl-v1.0\nsome arbitrary field\n1 2 3\n2 1 4"
            (makeTable [
                bimap Text.pack Text.pack <$> [("some","1"), ("arbitrary","2"), ("field","3")],
                bimap Text.pack Text.pack <$> [("some","2"), ("arbitrary","1"), ("field","4")]
            ])
            (4, "")
