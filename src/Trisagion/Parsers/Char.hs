{- |
Module: Trisagion.Parsers.Char

Parsers @('Streamable' s, 'ElementOf' s ~ Char) => 'Parser' s@.
-}

module Trisagion.Parsers.Char (
    -- * Types.
    -- * Newline parsers.
    lf,
    cr,

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

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor(..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser, takeWith)
import Trisagion.Parsers.ParseError (ValidationError)
import Trisagion.Parsers.Streamable (matchElem, satisfy)


{- | Parse a line feed (character @'\\n'@). -}
lf
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
lf = matchElem '\n'

{- | Parse a carriage return (character @'\\r'@). -}
cr
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
cr = matchElem '\r'


{- | Parse a, possibly null, prefix of whitespace. -}
spaces :: (Splittable s, ElementOf s ~ Char) => Parser s Void (PrefixOf s)
spaces = takeWith isSpace

{- | Parse a, possibly null, prefix of non-whitespace characters. -}
notSpaces :: (Splittable s, ElementOf s ~ Char) => Parser s Void (PrefixOf s)
notSpaces = takeWith (not . isSpace)


{- | Parse a decimal digit. -}
digit
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
digit = satisfy isDigit
