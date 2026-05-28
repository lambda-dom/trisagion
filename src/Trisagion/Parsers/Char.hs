{- |
Module: Trisagion.Parsers.Char

Parsers with constraints @'Streamable' Char s@.
-}

module Trisagion.Parsers.Char (
    -- * Types.
    Newline (..),
    Sign (..),

    -- * Error types.
    EscapeError (..),
    StringError (..),

    -- * Newline parsers.
    lf,
    cr,
    newline,
    line,

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
    string,
    comment,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (takeWhile)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isSpace, isDigit, ord, isLetter)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Utils.List (enumDown)
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser (Parser, validate, lookAhead)
import Trisagion.Parsers.Combinators (optional, manyTill)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), matchOne, one, eoi, satisfy)
import Trisagion.Parsers.Splittable (takeWith, takeWith1)


{- | The universal newline type. -}
data Newline
    = LF                                -- ^ Unix end of line.
    | CR                                -- ^ MacOS end of line.
    | CRLF                              -- ^ Windows end of line.
    deriving stock (Eq, Ord, Bounded, Enum, Show)

{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | The @EscapeError@ error type thrown by the 'escape' parser. -}
data EscapeError
    = EscapeCharError     !Char         -- ^ Error thrown on invalid escape character.
    | EscapeSequenceError !Char         -- ^ Error thrown on invalid escape sequence.
    deriving stock (Eq, Ord, Show)

{- | The @StringError@ error type thrown by the 'string' parser. -}
data StringError
    = StartQuoteError   !Char           -- ^ Error thrown on invalid opening quote.
    | EndQuoteError     !Char           -- ^ Error thrown on invalid ending quote.
    | StringEscapeError !EscapeError    -- ^ Error thrown on invalid escape sequence.
    deriving stock (Eq, Ord, Show)


{- | Parse a line feed (character @'\\n'@). -}
{-# INLINE lf #-}
lf :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Char
lf = matchOne '\n'

{- | Parse a carriage return (character @'\\r'@). -}
{-# INLINE cr #-}
cr :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Char
cr = matchOne '\r'

{- | Parse a universal newline from the stream. -}
{-# INLINE newline #-}
newline :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Newline
newline = do
    c <- first Right one
    case c of
        '\n' -> pure LF
        '\r' -> catchError (fmap (const CRLF) lf) (const (pure CR))
        _    -> throwError $ Left (ValidationError c)

{- | Parse a line from the stream. The line does not contain the ending newline and can be null. -}
{-# INLINEABLE line #-}
line :: forall b s . (Splittable Char b s, Monoid b) => Parser s InputError b
line = (fold . intersperse (singleton s '\r')) <$> do
        b <- first absurd eoi
        if b
            then throwError $ InputError 1
            else first absurd go
    where
        go :: Parser s Void [b] 
        go = do
            xs      <- takeWith (\ c -> c /= '\n' && c /= '\r')
            endline <- optional newline
            case endline of
                Just CR -> fmap (xs :) go
                _       -> pure [xs]


{- | Parse a, possibly null, prefix of whitespace. -}
{-# INLINE spaces #-}
spaces :: Splittable Char b s => Parser s Void b
spaces = takeWith isSpace

{- | Parse a, possibly null, prefix of non-whitespace characters. -}
{-# INLINE notSpaces #-}
notSpaces :: Splittable Char b s => Parser s Void b
notSpaces = takeWith (not . isSpace)


{- | Parse a decimal digit. -}
{-# INLINE digit #-}
digit :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Char
digit = satisfy isDigit

{- | Parse a number sign. -}
{-# INLINE sign #-}
sign :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Sign
sign = validate v one
    where
        v :: Char -> ValidationError Char :+: Sign
        v x = case x of
            '-' -> Right Negative
            '+' -> Right Positive
            _   -> Left $ ValidationError x

{- | Parse a positive 'Integer' in decimal format. -}
{-# INLINEABLE positive #-}
positive
    :: (Splittable Char b s, MonoFoldable Char b)
    => Parser s (ValidationError Char :+: InputError) Integer
positive = do
        digits <- takeWith1 isDigit
        let xs = enumDown (pred (monolength digits)) (monotoList digits)
        pure $ foldl' (+) 0 $ uncurry value <$> xs
    where
        value :: Word -> Char -> Integer
        -- Returns implementation-dependent garbage for non-decimal digits.
        value n c = fromIntegral (ord c - ord '0') * 10 ^ n

{- | Parse a signed 'Integer' in decimal format. -}
{-# INLINE integer #-}
integer
    :: (Splittable Char b s, MonoFoldable Char b)
    => Parser s (ValidationError Char :+: InputError) Integer
integer = do
    sgn <- first absurd (fromMaybe Positive <$> optional sign)
    case sgn of
        Positive -> positive
        Negative -> ((-1) *) <$> positive


{- | Parse a single (unicode) letter. -}
{-# INLINE letter #-}
letter :: Streamable Char s => Parser s (ValidationError Char :+: InputError) Char
letter = satisfy isLetter

{- | Parse a word. -}
{-# INLINE word #-}
word :: Splittable Char b s => Parser s (ValidationError Char :+: InputError) b
word = takeWith1 isLetter

{- | Parse an identifier.

An identifier is a letter followed by any combination of letters, digits and the characters @\'-\'@
or @\'_\'@.
-}
{-# INLINE identifier #-}
identifier :: Splittable Char b s => Parser s (ValidationError Char :+: InputError) b
identifier = do
        x <- first absurd $ lookAhead letter
        case x of
            Left e  -> throwError e
            Right _ -> first absurd $ takeWith p
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
escape :: forall b s . Splittable Char b s => Parser s (EscapeError :+: InputError) b
escape = do
        c <- first Right one
        if '\\' /= c
            then throwError . Left $ EscapeCharError c
            else validate v one
    where
        v :: Char -> EscapeError :+: b
        v c = case c of
            't'  -> Right (singleton s '\t')
            'n'  -> Right (singleton s '\n')
            'v'  -> Right (singleton s '\v')
            'f'  -> Right (singleton s '\f')
            'r'  -> Right (singleton s '\r')
            's'  -> Right (singleton s ' ')
            '\'' -> Right (singleton s '\'')
            '"'  -> Right (singleton s '"')
            '\\' -> Right (singleton s '\\')
            _    -> Left (EscapeSequenceError c)

{- | Parse a quoted string with escape sequences.

The quote characters are @\'\\\'\'@ and @\'\"\'@. The available escape sequences are the ones
accepted by the 'escape' parser.

note(s):

  * A quoted string does /not/ span multiple lines.
-}
{-# INLINEABLE string #-}
string
    :: (Splittable Char b s, Monoid b)
    => Parser s (StringError :+: InputError) b
string = do
        c      <- startQuote
        blocks <- manyTill (endQuote c) (catchError escapeSequence (const $ block c))
        pure $ foldl' (<>) mempty blocks
    where
        startQuote :: Streamable Char s => Parser s (StringError :+: InputError) Char
        startQuote = validate v one
            where
                v :: Char -> StringError :+: Char
                v c = if '\'' == c || '\"' == c
                    then Right c
                    else Left $ StartQuoteError c

        endQuote :: Streamable Char s => Char -> Parser s (StringError :+: InputError) Char
        endQuote c = validate v one
            where
                v :: Char -> StringError :+: Char
                v d = if c == d
                    then Right c
                    else Left $ EndQuoteError c

        escapeSequence :: Splittable Char b s => Parser s (StringError :+: InputError) b
        escapeSequence = first (first StringEscapeError) escape

        block :: Splittable Char b s => Char -> Parser s (StringError :+: InputError) b
        block c = first (first f) $ takeWith1 (\ d -> d /= c && d /= '\\' && d /= '\n')
            where
                f :: ValidationError Char -> StringError
                f (ValidationError d) = EndQuoteError d

{- | Parse a line comment.

A line comment starts with @p@ and runs to the end of the line (character @'\\n'@), returning the
prefix in-between.

note(s):

    * If the input stream contains Windows end of lines, then the comment text will contain an
    ending @'\\r'@. This can only be stripped by assuming more about the prefix @b@ (the practical
    solution) or complicating the implementation.
-}
{-# INLINE comment #-}
comment
    :: Splittable Char b s
    => Parser s e ()                    -- ^ Parser for start of line comment.
    -> Parser s e b
comment p = p *> first absurd (takeWith (/= '\n') <* optional lf)
