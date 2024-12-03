{- |
Module: Trisagion.Examples.Table

Parser for the .tbl tabular format.
-}

module Trisagion.Examples.Table (
    -- * Streams.
    Lines,

    -- ** Constructors.
    initLines,

    -- * Error types.
    TableError,

    -- * Parsers.
    parseHeader,
    parseFields,
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty, nonEmpty)

-- Libraries.
import Data.Text (Text)
import qualified Data.Text as Text (lines, pack, words, strip, null)

-- Package.
import Trisagion.Streams.Streamable (Stream, initialize)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Get (Get)
import Trisagion.Getters.Combinators (onParseError, validate)
import Trisagion.Getters.Streamable (InputError, ValidationError (..), one, matchElem)


{- | A type alias for @t'Stream' ['Text']@. -}
type Lines = Stream [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


{- | The @TableError@ error type. -}
data TableError
    = HeaderError
    | FieldsError
    deriving stock (Eq, Show)


{- | Parser for the header of a .tbl table. -}
parseHeader :: Get Lines (ParseError Lines TableError ) Text
parseHeader = onParseError HeaderError (matchElem (Text.pack "tbl-v1.0"))

{- | Get the next non-empty line, stripped of whitespace on both sides. -}
nextNonEmpty :: Get Lines (ParseError Lines InputError) Text
nextNonEmpty = do
    xs <- Text.strip <$> one
    if Text.null xs
        then nextNonEmpty
        else pure xs

{- | Parse the next non-empty line into a 'NonEmpty' of words. -}
parseLine :: Get Lines (ParseError Lines (Either InputError ValidationError)) (NonEmpty Text)
parseLine = validate (maybe (Left ValidationError) Right . nonEmpty) (Text.words <$> nextNonEmpty)

{- | Parser for the fields line of a .tbl table. -}
parseFields :: Get Lines (ParseError Lines TableError) (NonEmpty Text)
parseFields = onParseError FieldsError parseLine
