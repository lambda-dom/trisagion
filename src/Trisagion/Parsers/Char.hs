{- |
Module: Trisagion.Parsers.Char

Parsers with constraints @'Streamable' m Char s@.
-}

module Trisagion.Parsers.Char (
    -- * Types.
    Newline (..),
    Sign (..),

    -- * Error types.
    EscapeError (..),

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

    -- * Alphanumeric.
    letter,
    word,
    identifier,
    escape,
) where

-- Imports.
-- Base.
import Data.Char (isSpace, isDigit, ord, isLetter)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Utils.Integral (enumDown)
import Trisagion.Types.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.ParserT (ParserT, mapError, throw, catch, validate, lookAhead)
import Trisagion.Parsers.Combinators (optional)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), single, headP, satisfy)
import Trisagion.Parsers.Splittable (takeWhileP, takeWhile1)


{- | The universal newline type. -}
data Newline = LF | CR | CRLF
    deriving stock (Eq, Ord, Bounded, Enum, Show)

{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | The @EscapeError@ error type thrown by the 'escape' parser. -}
data EscapeError
    = EscapeCharError     !Char         -- ^ Error thrown on incorrect escape character.
    | EscapeSequenceError !Char         -- ^ Error thrown on incorrect escape sequence.
    deriving stock (Eq, Ord, Show)


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
        else if c == '\r'
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


{- | Parse a single (unicode) letter. -}
{-# INLINE letter #-}
letter :: Streamable m Char s => ParserT s (ValidationError Char :+: InputError) m Char
letter = satisfy isLetter

{- | Parse a word. -}
{-# INLINE word #-}
word :: Splittable m Char b s => ParserT s (ValidationError Char :+: InputError) m b
word = takeWhile1 isLetter

{- | Parse an identifier.

An identifier is a letter followed by any combination of letters, digits and the characters @\'-\'@
or @\'_\'@.
-}
{-# INLINE identifier #-}
identifier :: Splittable m Char b s => ParserT s (ValidationError Char :+: InputError) m b
identifier = do
        x <- mapError absurd $ lookAhead letter
        case x of
            Left e  -> throw e
            Right _ -> mapError absurd $ takeWhileP p
    where
        p :: Char -> Bool
        p c = isLetter c || isDigit c || '-' == c || '_' == c

{- | Parse an escape sequence.

The escape sequences currently supported are:

+----------+-----+---------------------------+
| Escape   | Ord | Meaning                   |
+==========+=====+===========================+
| @\\t@    | 9   | horizontal tab            |
+----------+-----+---------------------------+
| @\\n@    | 10  | new line                  |
+----------+-----+---------------------------+
| @\\v@    | 11  | vertical tab              |
+----------+-----+---------------------------+
| @\\f@    | 12  | form feed                 |
+----------+-----+---------------------------+
| @\\r@    | 13  | carriage return           |
+----------+-----+---------------------------+
| @\\s@    | 32  | space                     |
+----------+-----+---------------------------+
| @\\\'@   | 39  | single quote              |
+----------+-----+---------------------------+
| @\\\"@   | 34  | double quote              |
+----------+-----+---------------------------+
| @\\\\@   | 92  | character @\'\\\'@        |
+----------+-----+---------------------------+
-}
{-# INLINE escape #-}
escape :: forall m b s . Splittable m Char b s => ParserT s (EscapeError :+: InputError) m b
escape = do
        c <- mapError Right headP
        if '\\' /= c
            then throw (Left $ EscapeCharError c)
            else validate v headP
    where
        v :: Char -> EscapeError :+: b
        v c = case c of
            't'  -> Right (singleton m s '\t')
            'n'  -> Right (singleton m s '\n')
            'v'  -> Right (singleton m s '\v')
            'f'  -> Right (singleton m s '\f')
            'r'  -> Right (singleton m s '\r')
            's'  -> Right (singleton m s ' ')
            '\'' -> Right (singleton m s '\'')
            '"'  -> Right (singleton m s '"')
            '\\' -> Right (singleton m s '\\')
            _    -> Left (EscapeSequenceError c)
