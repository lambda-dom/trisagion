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
    line,
) where

-- Imports.
-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.ParserT (ParserT, mapError, throw, catch)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), single, headP, eoi)
import Trisagion.Typeclasses.Splittable (Splittable)
import Trisagion.Parsers.Combinators (optional)
import Data.Void (absurd, Void)
import Trisagion.Parsers.Splittable (takeWhileP)
import Data.Foldable (Foldable(..))


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


{- | Parse a line from the stream. The line does not contain the ending newline and can be null. -}
line :: forall m b s . (Splittable m Char b s, Monoid b) => ParserT s InputError m b 
line = do
        b <- mapError absurd eoi
        if b
            then throw $ InputError 1
            else fmap fold $ mapError absurd go
    where
        go :: ParserT s Void m [b] 
        go = do
            xs      <- takeWhileP (\ c -> c /= '\n' && c /= '\r')
            endline <- optional newline
            case endline of
                Just CR -> fmap (xs :) go
                _       -> pure [xs]
