{- |
Module: Trisagion.Examples.Table.Parsers

Auxiliary parsers for the .tbl tabular format.
-}

module Trisagion.Examples.Table.Parsers (
    -- * Streams. 
    Line,
    Lines,

    -- ** Constructors.
    initLines,

    -- * Error types.
    FieldsError (..),

    -- * Parsers.
    parseComment,
    parseFieldOrComment,
    parseHeader,
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
import Data.Text qualified as Text (lines, pack, strip, null)

-- Package.
import Trisagion.Streams.Counter (Counter, initialize)
import Trisagion.Streams.Offset (Offset)
import qualified Trisagion.Streams.Offset as Offset (initialize)
import Trisagion.Get (Get, eval)
import qualified Trisagion.Get as Getters (maybe)
import Trisagion.Getters.ParseError (Parser, validate)
import Trisagion.Getters.Streamable (InputError, MatchError, matchElem, one)
import Trisagion.Getters.Splittable (remainder)
import Trisagion.Getters.Char (notSpaces, spaces)


{- | A type alias for @t'Offset' 'Text'@. -}
type Line = Offset Text

{- | A type alias for @t'Counter' ['Text']@. -}
type Lines = Counter [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
{-# INLINE initLines #-}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


{- | The @FieldsError@ error type, raised on a line with no field values. -}
data FieldsError = FieldsError
    deriving stock (Eq, Show)


{- | Parser for a comment in a .tbl row. -}
{-# INLINE parseComment #-}
parseComment :: Parser Line (Either InputError (MatchError Char)) Text
parseComment = matchElem '#' *> first absurd remainder

{- | Parser for a either a comment or a field in a .tbl row. -}
{-# INLINE parseFieldOrComment #-}
parseFieldOrComment :: Get Line Void (Either Text Text)
parseFieldOrComment = Getters.maybe parseComment >>= maybe (Right <$> notSpaces) (pure . Left)

{- | Parser for the header of a .tbl table. -}
{-# INLINE parseHeader #-}
parseHeader :: Parser Lines (Either InputError (MatchError Text)) Text
parseHeader = matchElem (Text.pack "tbl-v1.0")

{- | Parser for a .tbl table row. -}
{-# INLINE parseLine #-}
parseLine :: Parser Lines InputError [Text]
parseLine = either absurd id . eval go . Offset.initialize . Text.strip <$> one
    where
        go :: Get Line Void [Text]
        go =
            spaces *> parseFieldOrComment >>=
                either
                    (const $ pure [])
                    (\ field -> if Text.null field then pure [] else (field :) <$> go)

{- | Parse one line as a 'NonEmpty' of field values. -}
{-# INLINE parseFields #-}
parseFields :: Parser Lines (Either InputError FieldsError) (NonEmpty Text)
parseFields = validate (maybe (Left FieldsError) Right . nonEmpty) parseLine
