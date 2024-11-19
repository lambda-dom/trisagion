{- |
Module: Trisagion.Getters.Char

Parsers @('Streamable' s, 'Element' s ~ Char) => 'Get' s@.
-}

module Trisagion.Getters.Char (
    -- * Types.
    Sign (..),
    
    -- * Newline parsers.
    lf,
    cr,

    -- * Whitespace parsers.
    spaces,

    -- * Numeric parsers.
    sign,
    digit,
    positive,
    signed,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.Char (isDigit, ord)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Data.MonoTraversable (MonoFoldable (..), Element)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Get (Get)
import Trisagion.Getters.Combinators (validate, option)
import Trisagion.Getters.Streamable (InputError, MatchError, ValidationError (..), matchElem, satisfy, one) 
import Trisagion.Getters.Splittable (takeWith, atLeastOneWith)


{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | Get a line feed (character @'\\n'@) from the stream. -}
lf
    :: (Streamable s, Element s ~ Char)
    => Get s (ParseError s (Either InputError (MatchError Char))) Char
lf = matchElem '\n'

{- | Get a carriage return (character @'\\r'@) from the stream. -}
cr
    :: (Streamable s, Element s ~ Char)
    => Get s (ParseError s (Either InputError (MatchError Char))) Char
cr = matchElem '\r'

{- | Parser for a, possibly empty, prefix of whitespace.

note(s):

    * Includes tabs but not newline characters.
-}
spaces :: (Splittable s, Element s ~ Char) => Get s Void (PrefixOf s)
spaces = takeWith (\c -> c == '\t' || c == ' ')

{- | Parser for the number sign. -}
sign
    :: (Streamable s, Element s ~ Char)
    => Get s (ParseError s (Either InputError ValidationError)) Sign
sign = validate v one
    where
        v x = case x of
            y | y == '-' -> Right Negative
            y | y == '+' -> Right Positive
            _            -> Left ValidationError

{- | Get a decimal digit from the streamable. -}
digit
    :: (Streamable s, Element s ~ Char)
    => Get s (ParseError s (Either InputError ValidationError)) Char
digit = satisfy isDigit

{- | Parser for positive, integer numbers in decimal format. -}
positive
    :: (Splittable s, MonoFoldable (PrefixOf s), Element s ~ Char, Element (PrefixOf s) ~ Char)
    => Get s (ParseError s (Either InputError ValidationError)) Word
positive = do
        digits <- atLeastOneWith isDigit
        let
             n = fromIntegral $ olength digits
             xs = zip [n - 1, n - 2 .. 0] (otoList digits)
        pure $ foldl' (+) 0 [ value d i | (i, d) <- xs]
    where
        value :: Char -> Word -> Word
        -- Returns implementation-dependent garbage for non-decimal digits.
        value c n = fromIntegral (ord c - ord '0') *  10 ^ n

{- | Transform a @'Word'@ parser into an @'Int'@-parser for signed numbers. -}
signed
    :: (Streamable s, Element s ~ Char)
    => Get s (ParseError s (Either InputError ValidationError)) Word
    -> Get s (ParseError s (Either InputError ValidationError)) Int
signed p = do
    sgn <- first absurd (fromMaybe Positive <$> option sign)
    number <- fromIntegral <$> p
    if sgn == Positive
    then pure number
    else pure (-number)
