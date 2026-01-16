{- |
Module: Trisagion.Parsers.Char

Parsers with constraints @'Streamable' m Char s@.
-}

module Trisagion.Parsers.Char (
    -- * Types.
    Newline (..),

    -- * Newline parsers.
    lf,
    cr,
    newline,

    -- * Whitespace parsers.
    spaces,
    notSpaces,

    -- * Numeric parsers.
    digit,
) where

-- Imports.
-- Base.
import Data.Char (isSpace, isDigit)
import Data.Void (Void)

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable)
import Trisagion.ParserT (ParserT, mapError, throw, catch)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), single, headP, satisfy)
import Trisagion.Parsers.Splittable (takeWhileP)


{- | The universal newline type. -}
data Newline = LF | CR | CRLF
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | Parse a line feed (character @'\\n'@). -}
{-# INLINE lf #-}
lf :: Streamable m Char s => ParserT s (ValidationError Char :+: InputError) m Char
lf = single '\n'

{- | Parse a carriage return (character @'\\r'@). -}
{-# INLINE cr #-}
cr :: Streamable m Char s => ParserT s (ValidationError Char :+: InputError) m Char
cr = single '\r'

{- | Parse a universal newline from the stream. -}
{-# INLINE newline #-}
newline :: Streamable m Char s => ParserT s (ValidationError Char :+: InputError) m Newline
newline = do
    c <- mapError Right headP
    if c == '\n'
    then pure LF
    else
        if c == '\r'
        then catch (fmap (const CRLF) lf) (const $ pure LF)
        else throw $ Left (ValidationError c)


{- | Parse a, possibly null, prefix of whitespace. -}
{-# INLINE spaces #-}
spaces :: Splittable m Char b s => ParserT s Void m b
spaces = takeWhileP isSpace

{- | Parse a, possibly null, prefix of non-whitespace characters. -}
{-# INLINE notSpaces #-}
notSpaces :: Splittable m Char b s => ParserT s Void m b
notSpaces = takeWhileP (not . isSpace)


{- | Parse a decimal digit. -}
{-# INLINE digit #-}
digit :: Streamable m Char s => ParserT s (ValidationError Char :+: InputError) m Char
digit = satisfy isDigit
