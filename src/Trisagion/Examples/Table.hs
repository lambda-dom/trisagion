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
    TableError (..),

    -- * Parsers.
    parseHeader,
    parseComment,
    parseLineWith,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Except (MonadError (..))
import Data.Text (Text)
import qualified Data.Text as Text (lines, pack, empty)

-- Package.
import Trisagion.Streams.Streamable (Stream, initialize)
import Trisagion.Types.ParseError (ParseError, makeParseError)
import Trisagion.Get (Get, eval)
import Trisagion.Getters.Combinators (onParseError)
import Trisagion.Getters.Streamable (one, matchElem, InputError, MatchError)
import Data.Void (Void, absurd)

{- | A type alias for @t'Stream' ['Text']@. -}
type Lines = Stream [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


{- | The @TableError@ error type. -}
data TableError e
    = HeaderError   -- ^ Error raised on incorrect table signature.
    | LineError e   -- ^ Error raised on a line parsing failure.
    deriving stock (Eq, Show, Functor)


{- | Parser for the header of a .tbl table. -}
parseHeader :: Get Lines (ParseError Lines (TableError e)) Text
parseHeader = onParseError HeaderError (matchElem (Text.pack "tbl-v1.0"))

{- | Parser for a comment in a .tbl row. -}
parseComment :: Get Text (ParseError Text (Either InputError (MatchError Char))) Text
parseComment = matchElem '#' *> first absurd remainder
    where
        remainder :: Get Text Void Text
        remainder = get <* put Text.empty

{- | Parse one line with a 'Text' parser.

note(s):

    * any unconsumed input by the 'Text' parser is discarded.
-}
parseLineWith 
    :: Get Text e a
    -> Get Lines (ParseError Lines (Either InputError (TableError e))) a
parseLineWith p = do
    s    <- get
    text <- first (fmap Left) one
    either
        (throwError . makeParseError s . Right . LineError)
        pure
        (eval p text)
