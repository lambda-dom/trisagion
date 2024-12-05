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
    parseFields,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (absurd)

-- Libraries.
import Control.Monad.State (gets)
import Data.Text (Text)
import qualified Data.Text as Text (lines, pack, empty, strip, words)

-- Package.
import Trisagion.Streams.Streamable (Stream, initialize)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Get (Get, eval)
import Trisagion.Getters.Combinators (replace, validate)
import Trisagion.Getters.Streamable (one, matchElem, InputError (..), MatchError)


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

{- | Parse one line as a possibly empty list of field, or column, names. -}
parseFields :: Get Lines (ParseError Lines InputError) [Text]
parseFields = first (fmap (either id absurd)) $ parseLineWith (gets Text.words)
