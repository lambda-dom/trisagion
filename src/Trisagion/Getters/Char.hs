{- |
Module: Trisagion.Getters.Char

Parsers @('HasPosition' s, 'Element' s ~ Char) => 'Get' s@.
-}

module Trisagion.Getters.Char (
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
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isDigit, ord, isSpace)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Data.MonoTraversable (MonoFoldable (..), Element)

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Get (Get)
import qualified Trisagion.Get as Getters (maybe)
import Trisagion.Getters.ParseError (Parser, ValidationError (..), validate)
import Trisagion.Getters.Streamable (InputError, MatchError, matchElem, satisfy, one) 
import Trisagion.Getters.Splittable (takeWith, atLeastOneWith)


{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | Get a line feed (character @'\\n'@) from the stream. -}
{-# INLINE lf #-}
lf
    :: (HasPosition s, Element s ~ Char)
    => Parser s (Either InputError (MatchError Char)) Char
lf = matchElem '\n'

{- | Get a carriage return (character @'\\r'@) from the stream. -}
{-# INLINE cr #-}
cr
    :: (HasPosition s, Element s ~ Char)
    => Parser s (Either InputError (MatchError Char)) Char
cr = matchElem '\r'

{- | Parser for a, possibly null, prefix of whitespace. -}
{-# INLINE spaces #-}
spaces :: (Splittable s, Element s ~ Char) => Get s Void (PrefixOf s)
spaces = takeWith isSpace

{- | Parser for a, possibly null, prefix of non-whitespace characters. -}
{-# INLINE notSpaces #-}
notSpaces :: (Splittable s, Element s ~ Char) => Get s Void (PrefixOf s)
notSpaces = takeWith (not . isSpace)

{- | Parser for the number sign. -}
{-# INLINE sign #-}
sign
    :: (HasPosition s, Element s ~ Char)
    => Parser s (Either InputError ValidationError) Sign
sign = validate v one
    where
        v x = case x of
            y | y == '-' -> Right Negative
            y | y == '+' -> Right Positive
            _            -> Left ValidationError

{- | Get a decimal digit from the streamable. -}
{-# INLINE digit #-}
digit
    :: (HasPosition s, Element s ~ Char)
    => Parser s (Either InputError ValidationError) Char
digit = satisfy isDigit

{- | Parser for positive, integer numbers in decimal format. -}
{-# INLINE positive #-}
positive
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), Element s ~ Char, Element (PrefixOf s) ~ Char)
    => Parser s (Either InputError ValidationError) Word
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
{-# INLINE signed #-}
signed
    :: (HasPosition s, Element s ~ Char)
    => Parser s (Either InputError ValidationError) Word
    -> Parser s (Either InputError ValidationError) Int
signed p = do
    sgn <- first absurd (fromMaybe Positive <$> Getters.maybe sign)
    number <- fromIntegral <$> p
    case sgn of
        Positive -> pure number
        Negative -> pure (-number)
