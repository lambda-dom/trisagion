{- |
Module: Trisagion.Examples.Table.Parsers

Parsers for the .tbl tabular format.
-}

module Trisagion.Examples.Table.Parsers (
    -- * Streams.
    Lines,

    -- ** Constructors.
    initLines,

    -- * Error types.
    FieldsError (..),
    MismatchError (..),

    -- * Parsers.
    parseHeader,
    parseComment,
    parseFieldOrComment,
    parseLine,
    parseFields,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Void (Void, absurd)

-- Libraries.
import Data.Text (Text)
import Data.Text qualified as Text (pack, lines, strip, null)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Streams.Streamable (Stream, initialize)
import Trisagion.Get (Get, eval)
import Trisagion.Getters.Combinators (validate)
import Trisagion.Getters.Combinators qualified as Getters (maybe)
import Trisagion.Getters.Streamable (InputError, MatchError, matchElem, one)
import Trisagion.Getters.Splittable (remainder)
import Trisagion.Getters.Char (notSpaces, spaces)


{- | A type alias for @t'Stream' ['Text']@. -}
type Lines = Stream [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


{- | The @FieldsError@ error type, raised on a line with no field values. -}
data FieldsError = FieldsError
    deriving stock (Eq, Show)

{- | The @MismatchError@ error type, raised on a line with incorrect number of fields. -}
data MismatchError = MismatchError
    deriving stock (Eq, Show)


{- | Parser for the header of a .tbl table. -}
parseHeader :: Get Lines (ParseError Lines (Either InputError (MatchError Text))) Text
parseHeader = matchElem (Text.pack "tbl-v1.0")

{- | Parser for a comment in a .tbl row. -}
parseComment :: Get Text (ParseError Text (Either InputError (MatchError Char))) Text
parseComment = matchElem '#' *> first absurd remainder

{- | Parser for a either a comment or a field in a .tbl row. -}
parseFieldOrComment :: Get Text Void (Either Text Text)
parseFieldOrComment = Getters.maybe parseComment >>= maybe (Right <$> notSpaces) (pure . Left)

{- | Parse one line stripped of whitespace on both ends with a 'Text' parser.

note(s):

    * any unconsumed input by the 'Text' parser is silently discarded.
-}
parseLineWith
    :: Get Text e a
    -> Get Lines (ParseError Lines (Either InputError e)) a
parseLineWith p = validate (eval p) (Text.strip <$> one)

{- | Parser for a .tbl table row. -}
parseLine :: Get Lines (ParseError Lines InputError) [Text]
parseLine = first (fmap (either id absurd)) $ parseLineWith go
    where
        go :: Get Text Void [Text]
        go =
            spaces *> parseFieldOrComment >>=
                either
                    (const $ pure [])
                    (\ field -> if Text.null field then pure [] else (field :) <$> go)

{- | Parse one line as a 'NonEmpty' of field values. -}
parseFields :: Get Lines (ParseError Lines (Either InputError FieldsError)) (NonEmpty Text)
parseFields = validate (maybe (Left FieldsError) Right . nonEmpty) parseLine
