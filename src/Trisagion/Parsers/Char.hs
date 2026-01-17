{- |
Module: Trisagion.Parsers.Char

Parsers with constraints @'Streamable' m Char s@.
-}

module Trisagion.Parsers.Char (
    -- * Types.
    Newline (..),
    Sign (..),

    -- * Newline parsers.
    lf,
    cr,
    newline,

    -- * Whitespace parsers.
    spaces,
    notSpaces,

    -- * Numeric parsers.
    digit,
    sign,
    positive,
    integer,
) where

-- Imports.
-- Base.
import Data.Char (isSpace, isDigit, ord)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Utils.Integral (enumDown)
import Trisagion.Types.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable)
import Trisagion.ParserT (ParserT, mapError, throw, catch, validate)
import Trisagion.Parsers.Combinators (optional)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), single, headP, satisfy)
import Trisagion.Parsers.Splittable (takeWhileP, takeWhile1)


{- | The universal newline type. -}
data Newline = LF | CR | CRLF
    deriving stock (Eq, Ord, Bounded, Enum, Show)

{- | The sign of a number. -}
data Sign = Negative | Positive
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

{- | Parse a number sign. -}
{-# INLINE sign #-}
sign :: Streamable m Char s => ParserT s (ValidationError Char :+: InputError) m Sign
sign = validate v headP
    where
        v :: Char -> ValidationError Char :+: Sign
        v x = case x of
            '-' -> Right Negative
            '+' -> Right Positive
            _   -> Left $ ValidationError x

{- | Parse a positive 'Integer' in decimal format. -}
{-# INLINEABLE positive #-}
positive
    :: (Splittable m Char b s, MonoFoldable Char b)
    => ParserT s (ValidationError Char :+: InputError) m Integer
positive = do
        digits <- takeWhile1 isDigit
        let xs = enumDown (pred (monolength digits)) (monotoList digits)
        pure $ foldl' (+) 0 $ uncurry value <$> xs
    where
        value :: Word -> Char -> Integer
        -- Returns implementation-dependent garbage for non-decimal digits.
        value n c = fromIntegral (ord c - ord '0') * 10 ^ n

{- | Parse a signed 'Integer' in decimal format. -}
{-# INLINE integer #-}
integer
    :: (Splittable m Char b s, MonoFoldable Char b)
    => ParserT s (ValidationError Char :+: InputError) m Integer
integer = do
    sgn <- mapError absurd (fromMaybe Positive <$> optional sign)
    case sgn of
        Positive -> positive
        Negative -> ((-1) *) <$> positive
