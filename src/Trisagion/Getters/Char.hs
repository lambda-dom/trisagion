{- |
Module: Trisagion.Getters.Char

Parsers @('Streamable' s, 'ElementOf' s ~ Char) => 'Get' s@.
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

    -- * Other lexemes.
    comment,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isDigit, ord, isSpace)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Data.MonoTraversable (MonoFoldable (..))

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.Streamable (Streamable, ElementOf)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Get (Get)
import qualified Trisagion.Get as Getters (maybe)
import Trisagion.Getters.ParseError (ValidationError (..), validate)
import Trisagion.Getters.Streamable (InputError, MatchError, matchElem, satisfy, one) 
import Trisagion.Getters.Splittable (takeWith, atLeastOneWith)


{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | Parse a line feed (character @'\\n'@). -}
{-# INLINE lf #-}
lf
    :: (Streamable s, ElementOf s ~ Char)
    => Get s (ParseError s (Either InputError (MatchError Char))) Char
lf = matchElem '\n'

{- | Parse a carriage return (character @'\\r'@). -}
{-# INLINE cr #-}
cr
    :: (Streamable s, ElementOf s ~ Char)
    => Get s (ParseError s (Either InputError (MatchError Char))) Char
cr = matchElem '\r'

{- | Parse a, possibly null, prefix of whitespace. -}
{-# INLINE spaces #-}
spaces :: (Splittable s, ElementOf s ~ Char) => Get s Void (PrefixOf s)
spaces = takeWith isSpace

{- | Parse a, possibly null, prefix of non-whitespace characters. -}
{-# INLINE notSpaces #-}
notSpaces :: (Splittable s, ElementOf s ~ Char) => Get s Void (PrefixOf s)
notSpaces = takeWith (not . isSpace)

{- | Parse a number sign. -}
{-# INLINE sign #-}
sign
    :: (Streamable s, ElementOf s ~ Char)
    => Get s (ParseError s (Either InputError ValidationError)) Sign
sign = validate v one
    where
        v x = case x of
            y | y == '-' -> Right Negative
            y | y == '+' -> Right Positive
            _            -> Left ValidationError

{- | Parse a decimal digit. -}
{-# INLINE digit #-}
digit
    :: (Streamable s, ElementOf s ~ Char)
    => Get s (ParseError s (Either InputError ValidationError)) Char
digit = satisfy isDigit

{- | Parse a positive, integer number in decimal format. -}
{-# INLINE positive #-}
positive
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ Char, ElementOf (PrefixOf s) ~ Char)
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
{-# INLINE signed #-}
signed
    :: (Streamable s, ElementOf s ~ Char)
    => Get s (ParseError s (Either InputError ValidationError)) Word
    -> Get s (ParseError s (Either InputError ValidationError)) Int
signed p = do
    sgn <- first absurd (fromMaybe Positive <$> Getters.maybe sign)
    number <- fromIntegral <$> p
    case sgn of
        Positive -> pure number
        Negative -> pure (-number)


{- | Parse a line comment.

A line comment starts with @p@ and then runs to the end of the line (character @'\\n'@), returning
the prefix in-between.

note(s):

    * If the streamable contains Windows end of lines, then the comment text will contain an ending
    @'\\r'@. This can only be stripped by assuming more about @'PrefixOf' s@ (the practical
    solution) or complicating and downgrading the implementation significantly.
-}
{-# INLINE comment #-}
comment
    :: (Splittable s, ElementOf s ~ Char)
    -- | Parser for beginning comment prefix.
    => Get s (ParseError s e) (PrefixOf s)  
    -> Get s (ParseError s e) (PrefixOf s)
comment p = do
    _ <- p
    first absurd $ takeWith (/= '\n') <* Getters.maybe cr
