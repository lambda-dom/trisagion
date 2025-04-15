{- |
Module: Trisagion.Parsers.Char

Parsers @('Streamable' s, 'ElementOf' s ~ Char) => 'Parser' s@.
-}

module Trisagion.Parsers.Char (
    -- * Types.
    Sign (..),

    -- * Newline parsers.
    lf,
    cr,

    -- * Whitespace parsers.
    spaces,
    notSpaces,

    -- * Numeric parsers.
    sign,
    digit,
    positive,
    signed,

    -- * Other lexemes.
    comment,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isSpace, isDigit, ord)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition)
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser (Parser, ParserPE, InputError, (:+:), takePrefixWith, one)
import qualified Trisagion.Parsers.Combinators as Parsers (maybe)
import Trisagion.Parsers.ParseError (ValidationError (..), validate)
import Trisagion.Parsers.Streamable (matchElem, satisfy)
import Trisagion.Parsers.Splittable (atLeastOneWith)


{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | Parse a line feed (character @'\\n'@). -}
lf
    :: (HasPosition s, Streamable s, ElementOf s ~ Char)
    => ParserPE s (InputError :+: ValidationError Char) Char
lf = matchElem '\n'

{- | Parse a carriage return (character @'\\r'@). -}
cr
    :: (HasPosition s, Streamable s, ElementOf s ~ Char)
    => ParserPE s (InputError :+: ValidationError Char) Char
cr = matchElem '\r'


{- | Parse a, possibly null, prefix of whitespace. -}
spaces :: (Splittable s, ElementOf s ~ Char) => Parser s Void (PrefixOf s)
spaces = takePrefixWith isSpace

{- | Parse a, possibly null, prefix of non-whitespace characters. -}
notSpaces :: (Splittable s, ElementOf s ~ Char) => Parser s Void (PrefixOf s)
notSpaces = takePrefixWith (not . isSpace)


{- | Parse a number sign. -}
sign
    :: (HasPosition s, Streamable s, ElementOf s ~ Char)
    => ParserPE s (InputError :+: ValidationError Char) Sign
sign = validate v one
    where
        v x = case x of
            y | y == '-' -> Right Negative
            y | y == '+' -> Right Positive
            _            -> Left $ ValidationError x

{- | Parse a decimal digit. -}
digit
    :: (HasPosition s, Streamable s, ElementOf s ~ Char)
    => ParserPE s (InputError :+: ValidationError Char) Char
digit = satisfy isDigit

{- | Parse a positive 'Integer' in decimal format. -}
positive
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ Char, ElementOf (PrefixOf s) ~ Char)
    => ParserPE s (InputError :+: ValidationError (PrefixOf s)) Integer
positive = do
        digits <- atLeastOneWith isDigit
        let
             n = fromIntegral $ monolength digits
             xs = zip [n - 1, n - 2 .. 0] (monotoList digits)
        pure $ foldl' (+) 0 [ value d i | (i, d) <- xs]
    where
        value :: Char -> Integer -> Integer
        -- Returns implementation-dependent garbage for non-decimal digits.
        value c n = fromIntegral (ord c - ord '0') *  10 ^ n

{- | Transform an 'Integer' parser into an 'Integer' parser for signed numbers. -}
signed
    :: (HasPosition s, Streamable s, ElementOf s ~ Char)
    => ParserPE s (InputError :+: ValidationError (PrefixOf s)) Integer
    -> ParserPE s (InputError :+: ValidationError (PrefixOf s)) Integer
signed p = do
    sgn <- first absurd (fromMaybe Positive <$> Parsers.maybe sign)
    number <- p
    case sgn of
        Positive -> pure number
        Negative -> pure (-number)

{- | Parse a line comment.

A line comment starts with @p@ and then runs to the end of the line (character @'\\n'@), returning
the prefix in-between.

note(s):

    * If the streamable contains Windows end of lines, then the comment text will contain an ending
    @'\\r'@. This can only be stripped by assuming more about @'PrefixOf' s@ (the practical
    solution) or complicating the implementation.
-}
comment
    :: (HasPosition s, Splittable s, ElementOf s ~ Char)
    => ParserPE s e (PrefixOf s)        -- ^ Parser for beginning comment prefix.
    -> ParserPE s e (PrefixOf s)
comment p = do
    _ <- p
    first absurd $ takePrefixWith (/= '\n') <* Parsers.maybe cr
