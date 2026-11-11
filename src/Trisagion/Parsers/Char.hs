{- |
Module: Trisagion.Parsers.Char

Parsers with constraints @'IStream' Char s@.
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
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- non-Hackage libraries.
import Data.MonoPointed (MonoPointed (..))
import Data.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Utils.List (enumDown)
import Trisagion.IStream (IStream)
import Trisagion.Split (Split (..))
import Trisagion.Parser (Parser, throw, catch)
import Trisagion.Parsers.Combinators (optional, manyTill, validate, lookAhead)
import Trisagion.Parsers.IStream (ValidationError (..), InputError (..), matchOne, one, eoi, satisfy)
import Trisagion.Parsers.Split (takeWith, takeWith1)


-- $setup
-- >>> import Trisagion.Parser
-- >>> import Trisagion.Parsers.Combinators
-- >>> import Trisagion.Parsers.IStream


{- | The universal newline type. -}
data Newline
    = LF                                -- ^ Unix end of line.
    | CR                                -- ^ MacOS end of line. Mainly for backwards compatibility.
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


{- | Parse a line feed (character @'\\n'@).

=== __Examples:__

>>> parse lf "\n123"
Right ('\n',"123")

>>> parse lf "0123"
Left (Right (ValidationError '0'))

>>> parse lf ""
Left (Left (InputError 1))
-}
{-# INLINE lf #-}
lf :: IStream Char s => Parser s (InputError :+: ValidationError Char) Char
lf = matchOne '\n'

{- | Parse a carriage return (character @'\\r'@). -}
{-# INLINE cr #-}
cr :: IStream Char s => Parser s (InputError :+: ValidationError Char) Char
cr = matchOne '\r'

{- | Parse a universal newline from the input stream.

=== __Examples:__

>>> parse newline "\n123"
Right (LF,"123")

>>> parse newline "\r123"
Right (CR,"123")

>>> parse newline "\r\n123"
Right (CRLF,"123")

>>> parse newline "\n\r123"
Right (LF,"\r123")

>>> parse newline "123"
Left (Right (ValidationError '1'))

>>> parse newline ""
Left (Left (InputError 1))
-}
{-# INLINE newline #-}
newline :: IStream Char s => Parser s (InputError :+: ValidationError Char) Newline
newline = do
    c <- first Left one
    case c of
        '\n' -> pure LF
        '\r' -> catch (fmap (const CRLF) lf) (const (pure CR))
        _    -> throw $ Right (ValidationError c)

{- | Parse a line from the input stream. The line does not contain the ending newline and can be null.

note(s):

    * Either @'\\n'@ or @"\\r\\n"@ count as a newline; @'\\r'@ does /not/.

=== __Examples:__

>>> parse line "0123\n456"
Right ("0123","456")

>>> parse line "0123\r\n456"
Right ("0123","456")

>>> parse line "0123\r456"
Right ("0123\r456","")

>>> parse line "0123\n\n456"
Right ("0123","\n456")

>>> parse line "\n456"
Right ("","456")

>>> parse line "456"
Right ("456","")

>>> parse line ""
Left (InputError 1)
-}
{-# INLINEABLE line #-}
line :: forall b s . (MonoPointed Char b, Split Char b s, Monoid b) => Parser s InputError b
line = (foldl' (<>) mempty . intersperse (monopoint '\r')) <$> do
        b <- first absurd eoi
        if b
            then throw $ InputError 1
            else first absurd go
    where
        go :: Parser s Void [b] 
        go = do
            xs      <- takeWith (\ c -> c /= '\n' && c /= '\r')
            endline <- optional newline
            case endline of
                Just CR -> fmap (xs :) go
                _       -> pure [xs]


{- | Parse a, possibly null, prefix of whitespace.

=== __Examples:__

>>> parse spaces "  123"
Right ("  ","123")

>>> parse spaces "\v\f\r\n123"
Right ("\v\f\r\n","123")

>>> parse spaces "0123"
Right ("","0123")

>>> parse spaces ""
Right ("","")
-}
{-# INLINE spaces #-}
spaces :: Split Char b s => Parser s Void b
spaces = takeWith isSpace

{- | Parse a, possibly null, prefix of non-whitespace characters. -}
{-# INLINE notSpaces #-}
notSpaces :: Split Char b s => Parser s Void b
notSpaces = takeWith (not . isSpace)


{- | Parse a decimal digit. -}
{-# INLINE digit #-}
digit :: IStream Char s => Parser s (InputError :+: ValidationError Char) Char
digit = satisfy isDigit

{- | Parse a number sign.

=== __Examples:__

>>> parse sign "+123"
Right (Positive,"123")

>>> parse sign "-123"
Right (Negative,"123")

>>> parse sign "0123"
Left (Right (ValidationError '0'))

>>> parse sign ""
Left (Left (InputError 1))
-}
{-# INLINE sign #-}
sign :: IStream Char s => Parser s (InputError :+: ValidationError Char) Sign
sign = validate v one
    where
        v :: Char -> ValidationError Char :+: Sign
        v x = case x of
            '-' -> Right Negative
            '+' -> Right Positive
            _   -> Left $ ValidationError x

{- | Parse a positive 'Integer' in decimal format.

note(s):

    * This parser can construct an arbitrarily large 'Integer' which can cause memory exhaustion.

=== __Examples:__

>>> parse positive "123"
Right (123,"")

>>> parse positive "1ab"
Right (1,"ab")

>>> parse positive "00123"
Right (123,"")

>>> parse positive "abc"
Left (Right (ValidationError 'a'))

>>> parse positive ""
Left (Left (InputError 1))
-}
{-# INLINEABLE positive #-}
positive
    :: (Split Char b s, MonoFoldable Char b)
    => Parser s (InputError :+: ValidationError Char) Integer
positive = do
        digits <- takeWith1 isDigit
        let xs = enumDown (pred (monolength digits)) (monotoList digits)
        pure $ foldl' (+) 0 $ uncurry value <$> xs
    where
        value :: Int -> Char -> Integer
        -- Returns implementation-dependent garbage for non-decimal digits.
        value n c = fromIntegral (ord c - ord '0') * 10 ^ n

{- | Parse a signed 'Integer' in decimal format.

note(s):

    * This parser can construct an arbitrarily large 'Integer' which can cause memory exhaustion.

=== __Examples:__

>>> parse integer "123"
Right (123,"")

>>> parse integer "00123"
Right (123,"")

>>> parse integer "+123"
Right (123,"")

>>> parse integer "-123"
Right (-123,"")
-}
{-# INLINE integer #-}
integer
    :: (Split Char b s, MonoFoldable Char b)
    => Parser s (InputError :+: ValidationError Char) Integer
integer = do
    sgn <- first absurd (fromMaybe Positive <$> optional sign)
    case sgn of
        Positive -> positive
        Negative -> negate <$> positive


{- | Parse a single (unicode) letter. -}
{-# INLINE letter #-}
letter :: IStream Char s => Parser s (InputError :+: ValidationError Char) Char
letter = satisfy isLetter

{- | Parse a word.

=== __Examples:__

>>> parse word "abc  "
Right ("abc","  ")

>>> parse word "abc__  "
Right ("abc","__  ")

>>> parse word "__abc"
Left (Right (ValidationError '_'))

>>> parse word "  __abc"
Left (Right (ValidationError ' '))
-}
{-# INLINE word #-}
word :: Split Char b s => Parser s (InputError :+: ValidationError Char) b
word = takeWith1 isLetter

{- | Parse an identifier.

An identifier is a letter followed by any combination of letters, digits and the characters @\'-\'@
or @\'_\'@.

=== __Examples:__

>>> parse identifier "abc  "
Right ("abc","  ")

>>> parse identifier "abc__  "
Right ("abc__","  ")

>>> parse identifier "__abc"
Left (Right (ValidationError '_'))

>>> parse identifier "  __abc"
Left (Right (ValidationError ' '))
-}
{-# INLINE identifier #-}
identifier :: Split Char b s => Parser s (InputError :+: ValidationError Char) b
identifier = do
        x <- first absurd $ lookAhead letter
        case x of
            Left e  -> throw e
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

=== __Examples:__

The examples are run in ghci so the escape character @\'\\\'@ must itself be escaped.

>>> parse escape "\\n"
Right ("\n","")

>>> parse escape "\\s"
Right (" ","")

>>> parse escape "\\\\"
Right ("\\","")

>>> parse escape "s"
Left (Right (EscapeCharError 's'))

>>> parse escape "\\a"
Left (Right (EscapeSequenceError 'a'))

>>> parse escape ""
Left (Left (InputError 1))
-}
{-# INLINE escape #-}
escape
    :: forall b s . (MonoPointed Char b, Split Char b s)
    => Parser s (InputError :+: EscapeError) b
escape = do
        c <- first Left one
        if '\\' /= c
            then throw . Right $ EscapeCharError c
            else validate v one
    where
        v :: Char -> EscapeError :+: b
        v c = case c of
            't'  -> Right (monopoint '\t')
            'n'  -> Right (monopoint '\n')
            'v'  -> Right (monopoint '\v')
            'f'  -> Right (monopoint '\f')
            'r'  -> Right (monopoint '\r')
            's'  -> Right (monopoint ' ')
            '\'' -> Right (monopoint '\'')
            '"'  -> Right (monopoint '"')
            '\\' -> Right (monopoint '\\')
            _    -> Left (EscapeSequenceError c)

{- | Parse a quoted string with escape sequences.

The quote characters are @\'\\\'\'@ and @\'\"\'@. The available escape sequences are the ones
accepted by the 'escape' parser.

note(s):

  * A quoted string does /not/ span multiple lines.

=== __Examples:__

The examples are run in ghci so the escape character @\'\\\'@ must itself be escaped.

>>> parse string "'Quoted string.'"
Right ("Quoted string.","")

>>> parse string "'Quoted string with many \\s\\s\\s spaces.'"
Right ("Quoted string with many     spaces.","")

>>> parse string "'Quoted string with many\\s\\s\\sspaces and one \\\'\\\\\\\' escape character.'"
Right ("Quoted string with many   spaces and one '\\' escape character.","")
-}
{-# INLINEABLE string #-}
string
    :: (MonoPointed Char b, Split Char b s, Monoid b)
    => Parser s (InputError :+: StringError) b
string = do
        c      <- startQuote
        blocks <- manyTill (endQuote c) (catch escapeSequence (const $ block c))
        pure $ foldl' (<>) mempty blocks
    where
        startQuote :: IStream Char s => Parser s (InputError :+: StringError) Char
        startQuote = validate v one
            where
                v :: Char -> StringError :+: Char
                v c = if '\'' == c || '\"' == c
                    then Right c
                    else Left $ StartQuoteError c

        endQuote :: IStream Char s => Char -> Parser s (InputError :+: StringError) Char
        endQuote c = validate v one
            where
                v :: Char -> StringError :+: Char
                v d = if c == d
                    then Right c
                    else Left $ EndQuoteError c

        escapeSequence :: (MonoPointed Char b, Split Char b s) => Parser s (InputError :+: StringError) b
        escapeSequence = first (fmap StringEscapeError) escape

        block :: Split Char b s => Char -> Parser s (InputError :+: StringError) b
        block c = first (fmap f) $ takeWith1 (\ d -> d /= c && d /= '\\' && d /= '\n')
            where
                f :: ValidationError Char -> StringError
                f (ValidationError d) = EndQuoteError d

{- | Parse a line comment.

A line comment starts with @p@ and runs to the end of the line, returning the prefix in-between.

note(s):

    * Either @'\\n'@ or @"\\r\\n"@ count as a newline; @'\\r'@ does /not/.

=== __Examples:__

>>> parse (comment (skip $ matchOne '#')) "#a line comment\r\n code starts here"
Right ("a line comment"," code starts here")
-}
{-# INLINE comment #-}
comment
    :: (Split Char b s, MonoPointed Char b, Monoid b)
    => Parser s e ()                    -- ^ Parser for start of line comment.
    -> Parser s (InputError :+: e) b
comment p = first Right p *> first Left line
