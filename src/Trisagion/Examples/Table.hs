{- |
Module: Trisagion.Examples.Table

Parser for the .tbl tabular format.
-}

module Trisagion.Examples.Table (
    -- * Streams.
    Lines,

    -- ** Constructors.
    initLines,

    -- * Generic parsers.
    parseLineWith,

    -- * Auxiliary parsers.
    parseHeader,
    parseComment,
    parseFieldOrComment,
    parseRow,
    parseFields,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (gets)
import Data.Text (Text)
import qualified Data.Text as Text (lines, pack, empty, strip, words, null)

-- Package.
import Trisagion.Streams.Streamable (Stream, initialize)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Get (Get, eval)
import Trisagion.Getters.Combinators (replace, validate,option)
import Trisagion.Getters.Streamable (one, matchElem, InputError (..), MatchError)
import Trisagion.Getters.Char (notSpaces, spaces)


{- | A type alias for @t'Stream' ['Text']@. -}
type Lines = Stream [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


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
                Left _     -> pure []
                Right field ->
                    if Text.null field
                        then pure []
                        else (field :) <$> go

{- | Parse one line as a possibly empty list of field, or column, names. -}
parseFields :: Get Lines (ParseError Lines InputError) [Text]
parseFields = first (fmap (either id absurd)) $ parseLineWith (gets Text.words)
