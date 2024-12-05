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
    SignatureError (..),
    LineError (..),

    -- * Parsers.
    parseHeader,
    parseComment,
    parseLineWith,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity (..))
import Data.Void (absurd)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Except (MonadError (..))
import Data.Text (Text)
import qualified Data.Text as Text (lines, pack, empty)

-- Package.
import Trisagion.Streams.Streamable (Stream, initialize)
import Trisagion.Types.ParseError (ParseError, makeParseError)
import Trisagion.Get (Get, eval, replace)
import Trisagion.Getters.Combinators (onParseError)
import Trisagion.Getters.Streamable (one, matchElem, InputError, MatchError)

{- | A type alias for @t'Stream' ['Text']@. -}
type Lines = Stream [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


{- | The @HeaderError@ error type. -}
data SignatureError = SignatureError
    deriving stock (Eq, Show)

{- | The @LineError@ error type. -}
newtype LineError e = LineError e
    deriving stock (Eq, Show)
    deriving Functor via Identity


{- | Parser for the header of a .tbl table. -}
parseHeader :: Get Lines (ParseError Lines SignatureError) Text
parseHeader = onParseError SignatureError (matchElem (Text.pack "tbl-v1.0"))

{- | Parser for a comment in a .tbl row. -}
parseComment :: Get Text (ParseError Text (Either InputError (MatchError Char))) Text
parseComment = matchElem '#' *> first absurd (replace Text.empty)

{- | Parse one line with a 'Text' parser.

note(s):

    * any unconsumed input by the 'Text' parser is discarded.
-}
parseLineWith 
    :: Get Text e a
    -> Get Lines (ParseError Lines (Either InputError (LineError e))) a
parseLineWith p = do
    s    <- get
    text <- first (fmap Left) one
    either
        (throwError . makeParseError s . Right . LineError)
        pure
        (eval p text)
