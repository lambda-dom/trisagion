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
    EmptyLineError (..),
    MismatchError (..),
    TableError (..),

    -- * Generic parsers.
    parseLineWith,

    -- * Parsers.
    parseTable,

    -- ** Auxiliary parsers.
    parseHeader,
    parseComment,
    parseFieldOrComment,
    parseRow,
    parseFields,
    parseRows,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..))
import Data.Text (Text)
import qualified Data.Text as Text (lines, pack, empty, strip, null)

-- Package.
import Trisagion.Lib.NonEmpty (zipExact)
import Trisagion.Streams.Streamable (Stream, initialize)
import Trisagion.Types.ParseError (ParseError, makeParseError)
import Trisagion.Get (Get, eval)
import Trisagion.Getters.Combinators (replace, validate,option, onParseError)
import Trisagion.Getters.Streamable (one, matchElem, InputError (..), MatchError)
import Trisagion.Getters.Char (notSpaces, spaces)


{- | A type alias for @t'Stream' ['Text']@. -}
type Lines = Stream [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


{- | The @EmptyLineError@ error type, raised on an empty line. -}
data EmptyLineError = EmptyLineError
    deriving stock (Eq, Show)

{- | The @MismatchError@ error type, raised when the number of values and field names are not equal. -}
data MismatchError = MismatchError
    deriving stock (Eq, Show)

{- | The @TableError@ error type. -}
data TableError
    = HeaderError
    | FieldsError
    | RowError
    deriving stock (Eq, Show)


{- | Parse one line stripped of whitespace on both ends with a 'Text' parser.

note(s):

    * any unconsumed input by the 'Text' parser is discarded.
-}
parseLineWith
    :: Get Text e a
    -> Get Lines (ParseError Lines (Either InputError e)) a
parseLineWith p = validate (eval p) (Text.strip <$> one)

{- | Parser for the header of a .tbl table. -}
parseHeader :: Get Lines (ParseError Lines (Either InputError (MatchError Text))) Text
parseHeader = matchElem (Text.pack "tbl-v1.0")

{- | Parser for a comment in a .tbl row. -}
parseComment :: Get Text (ParseError Text (Either InputError (MatchError Char))) Text
parseComment = matchElem '#' *> first absurd (replace (const Text.empty))

{- | Parser for a either a comment or a field in a .tbl row. -}
parseFieldOrComment :: Get Text Void (Either Text Text)
parseFieldOrComment = option parseComment >>= maybe (Right <$> notSpaces) (pure . Left)

{- | Parser for a .tbl table row. -}
parseRow :: Get Lines (ParseError Lines InputError) [Text]
parseRow = first (fmap (either id absurd)) $ parseLineWith go
    where
        go :: Get Text Void [Text]
        go = do
            r <- spaces *> parseFieldOrComment
            case r of
                Left _      -> pure []
                Right field ->
                    if Text.null field
                        then pure []
                        else (field :) <$> go

{- | Parse one line as a 'NonEmpty' of field names. -}
parseFields :: Get Lines (ParseError Lines (Either InputError EmptyLineError)) (NonEmpty Text)
parseFields = validate (maybe (Left EmptyLineError) Right . nonEmpty) parseRow

{- | Parser for a non-empty list of .tbl table rows. -}
parseRows :: NonEmpty Text -> Get Lines (ParseError Lines TableError) [NonEmpty (Text, Text)]
parseRows fields = go
    where
        go = do
            s <- get
            r <- first absurd $ option parseRow
            case r of
                Nothing -> pure []
                Just ts ->
                    case nonEmpty ts of
                        -- Case of empty line.
                        Nothing -> go
                        Just fs ->
                            case zipExact fields fs of
                                Nothing -> throwError $ makeParseError s RowError
                                Just ps -> (ps :) <$> go

{- | Parser for an entire .tbl table. -}
parseTable
    :: (NonEmpty (Text, Text) -> a)
    -> Get Lines (ParseError Lines TableError) [a]
parseTable processRow = do
        _  <- onParseError HeaderError parseHeader
        onParseError FieldsError parseFields >>= fmap (fmap processRow) . parseRows
