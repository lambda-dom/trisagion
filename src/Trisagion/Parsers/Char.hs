{- |
Module: Trisagion.Parsers.Char

Parsers with constraints @'Streamable' Char s@.
-}

module Trisagion.Parsers.Char (
    -- * Types.
    Newline (..),
    Sign (..),

    -- * Error types.
    EscapeError (..),
    StringError (..),

    -- * Newline parsers.
    lf,
    cr,
    newline,
    line,

    -- * Whitespace parsers.
    spaces,
    notSpaces,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (takeWhile)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser (Parser, throw)
import Trisagion.Parsers.Combinators (optional)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), single, one, eoi)
import Trisagion.Parsers.Splittable (takeWhile)


{- | The universal newline type. -}
data Newline
    = LF                                -- ^ Unix end of line.
    | CR                                -- ^ MacOS end of line.
    | CRLF                              -- ^ Windows end of line.
    deriving stock (Eq, Ord, Bounded, Enum, Show)

{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | The @EscapeError@ error type thrown by the 'escape' parser. -}
data EscapeError
    = EscapeCharError     !Char         -- ^ Error thrown on invalid escape character.
    | EscapeSequenceError !Char         -- ^ Error thrown on invalid escape sequence.
    deriving stock (Eq, Ord, Show)

{- | The @StringError@ error type thrown by the 'string' parser. -}
data StringError
    = StartQuoteError   !Char           -- ^ Error thrown on invalid opening quote.
    | EndQuoteError     !Char           -- ^ Error thrown on invalid ending quote.
    | StringEscapeError !EscapeError    -- ^ Error thrown on invalid escape sequence.
    deriving stock (Eq, Ord, Show)


{- | Parse a line feed (character @'\\n'@). -}
{-# INLINE lf #-}
lf :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Char
lf = single '\n'

{- | Parse a carriage return (character @'\\r'@). -}
{-# INLINE cr #-}
cr :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Char
cr = single '\r'

{- | Parse a universal newline from the stream. -}
{-# INLINE newline #-}
newline :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Newline
newline = do
    c <- first Right one
    case c of
        '\n' -> pure LF
        '\r' -> catchError (fmap (const CRLF) lf) (const (pure CR))
        _    -> throw $ Left (ValidationError c)

{- | Parse a line from the stream. The line does not contain the ending newline and can be null. -}
{-# INLINEABLE line #-}
line :: forall b s . (Splittable Char b s, Monoid b) => Parser s InputError b
line = (fold . intersperse (singleton s '\r')) <$> do
        b <- first absurd eoi
        if b
            then throw (InputError 1)
            else first absurd go
    where
        go :: Parser s Void [b] 
        go = do
            xs      <- takeWhile (\ c -> c /= '\n' && c /= '\r')
            endline <- optional newline
            case endline of
                Just CR -> fmap (xs :) go
                _       -> pure [xs]

{- | Parse a, possibly null, prefix of whitespace. -}
{-# INLINE spaces #-}
spaces :: Splittable Char b s => Parser s Void b
spaces = takeWhile isSpace

{- | Parse a, possibly null, prefix of non-whitespace characters. -}
{-# INLINE notSpaces #-}
notSpaces :: Splittable Char b s => Parser s Void b
notSpaces = takeWhile (not . isSpace)

