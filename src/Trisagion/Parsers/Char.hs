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
) where

-- Imports.
-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.ParserT (ParserT, mapError, throw, catch)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError, single, headP)


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
