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
-- Base.
import Data.Bifunctor (bimap)
import Data.Char (isSpace, isDigit, ord, isLetter)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Utils.Integral (enumDown)
import Trisagion.Types.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (singleton))
import Trisagion.ParserT (ParserT, mapError, throw, validate, lookAhead, catch)
import Trisagion.Parsers.Combinators (optional, manyTill)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), single, headP, satisfy, eoi)
import Trisagion.Parsers.Splittable (takeWhileP, takeWhile1)


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
    case c of
        '\n' -> pure LF
        '\r' -> catch (fmap (const CRLF) lf) (const (pure CR))
        _    -> throw $ Left (ValidationError c)

{- | Parse a line from the stream. The line does not contain the ending newline and can be null. -}
{-# INLINEABLE line #-}
line :: forall m b s . (Splittable m Char b s, Monoid b) => ParserT s InputError m b
line = (fold . intersperse (singleton m s '\r')) <$> do
        b <- mapError absurd eoi
        if b
            then throw (InputError 1)
            else mapError absurd go
    where
        go :: ParserT s Void m [b] 
        go = do
            xs      <- takeWhileP (\ c -> c /= '\n' && c /= '\r')
            endline <- optional newline
            case endline of
                Just CR -> fmap (xs :) go
                _       -> pure [xs]


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

{- | Parse a quoted string with escape sequences.

The quote characters are @\'\\\'\'@ and @\'\"\'@. The available escape sequences are the ones
accepted by the 'escape' parser.

note(s):

  * A quoted string does /not/ span multiple lines.
-}
{-# INLINEABLE string #-}
string
    :: (Splittable m Char b s, Monoid b)
    => ParserT s (StringError :+: InputError) m b
string = do
        c      <- startQuote
        blocks <- manyTill (endQuote c) (catch escapeSequence (const $ block c))
        pure $ foldl' (<>) mempty blocks
    where
        startQuote :: Streamable m Char s => ParserT s (StringError :+: InputError) m Char
        startQuote = validate v headP
            where
                v :: Char -> StringError :+: Char
                v c = if '\'' == c || '\"' == c
                    then Right c
                    else Left $ StartQuoteError c

        endQuote :: Streamable m Char s => Char -> ParserT s (StringError :+: InputError) m Char
        endQuote c = validate v headP
            where
                v :: Char -> StringError :+: Char
                v d = if c == d
                    then Right c
                    else Left $ EndQuoteError c

        escapeSequence :: Splittable m Char b s => ParserT s (StringError :+: InputError) m b
        escapeSequence = mapError (bimap StringEscapeError id) escape

        block :: Splittable m Char b s => Char -> ParserT s (StringError :+: InputError) m b
        block c = mapError (bimap f id) $ takeWhile1 (\ d -> d /= c && d /= '\\' && d /= '\n')
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
    :: Splittable m Char b s
    => ParserT s e m ()                 -- ^ Parser for start of line comment.
    -> ParserT s e m b
comment p = p *> mapError absurd (takeWhileP (/= '\n') <* optional lf)
