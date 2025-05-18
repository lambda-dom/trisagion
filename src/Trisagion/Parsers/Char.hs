{- |
Module: Trisagion.Parsers.Char

Parsers @('HasOffset' s, 'ElementOf' s ~ Char) => 'Parser' s@.
-}

module Trisagion.Parsers.Char (
    -- * Error types.
    QuoteError (..),
    EscapeError (..),
    StringError (..),

    -- * Types.
    Sign (..),

    -- * Newline parsers.
    lf,
    cr,

    -- * Whitespace parsers.
    spaces,
    notSpaces,

    -- * Numeric parsers.
    digit,
    positive,
    sign,
    integer,

    -- * Alphanumeric.
    letter,
    word,
    identifier,
    escape,
    string,

    -- * Other lexemes.
    comment,
) where

-- Imports.
-- Base.
import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isSpace, isDigit, ord, isLetter)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor(..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Lib.Utils (enumDown)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError, ValidationError)
import Trisagion.Parser
import qualified Trisagion.Parsers.Combinators as Combinators (maybe)
import Trisagion.Parsers.Combinators (manyTill)


{- | The 'QuoteError' error tag type thrown on quoting errors. -}
data QuoteError
    -- | Opening quote error.
    = StartQuoteError {-# UNPACK #-} !Char
    -- | Ending quote error.
    | EndQuoteError {-# UNPACK #-} !Char
    deriving stock (Eq, Ord, Show)


{- | The 'EscapeError' error tag type thrown by the 'escape' parser. -}
data EscapeError
    -- | Error thrown on incorrect escape character.
    = EscapeCharError {-# UNPACK #-} !Char
    -- | Error thrown on incorrect escape sequence.
    | EscapeSequenceError {-# UNPACK #-} !Char
    deriving stock (Eq, Ord, Show)


{- | The 'StringError' error tag type thrown by the 'string' parser.

There is only one value of this type, so all the discriminating information is in the backtrace.
-}
data StringError = StringError
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | Parse a line feed (character @'\\n'@).

=== __Examples:__

>>> parse lf (initialize "\n123")
Right ('\n',Counter 1 "123")

>>> parse lf (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])

>>> parse lf (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE lf #-}
lf
    :: (HasOffset s, ElementOf s ~ Char)
    => Parser s (ParseError (ValidationError Char)) Char
lf = matchOne '\n'

{- | Parse a carriage return (character @'\\r'@). -}
{-# INLINE cr #-}
cr
    :: (HasOffset s, ElementOf s ~ Char)
    => Parser s (ParseError (ValidationError Char)) Char
cr = matchOne '\r'


{- | Parse a, possibly null, prefix of whitespace.

=== __Examples:__

>>> parse spaces (initialize "  123")
Right ("  ",Counter 2 "123")

>>> parse spaces (initialize "\v\f\r\n123")
Right ("\v\f\r\n",Counter 4 "123")

>>> parse spaces (initialize "0123")
Right ("",Counter 0 "0123")

>>> parse spaces (initialize "")
Right ("",Counter 0 "")
-}
{-# INLINE spaces #-}
spaces :: (Splittable s, ElementOf s ~ Char) => Parser s Void (PrefixOf s)
spaces = takeWith isSpace

{- | Parse a, possibly null, prefix of non-whitespace characters. -}
{-# INLINE notSpaces #-}
notSpaces :: (Splittable s, ElementOf s ~ Char) => Parser s Void (PrefixOf s)
notSpaces = takeWith (not . isSpace)


{- | Parse a decimal digit. -}
{-# INLINE digit #-}
digit
    :: (HasOffset s, ElementOf s ~ Char)
    => Parser s (ParseError (ValidationError Char)) Char
digit = satisfy isDigit

{- | Parse a positive 'Integer' in decimal format.

=== __Examples:__

>>> parse positive (initialize "123")
Right (123,Counter 3 "")

>>> parse positive (initialize "1ab")
Right (1,Counter 1 "ab")

>>> parse positive (initialize "00123")
Right (123,Counter 5 "")

>>> parse positive (initialize "abc")
Left (Cons (ErrorItem 1 (ValidationError 'a')) [])

>>> parse positive (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINEABLE positive #-}
positive
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ Char, ElementOf (PrefixOf s) ~ Char)
    => Parser s (ParseError (ValidationError (ElementOf s))) Integer
positive = do
        digits <- takeWith1 isDigit
        let xs = enumDown (pred (monolength digits)) (monotoList digits)
        pure $ foldl' (+) 0 $ uncurry value <$> xs
    where
        value :: Word -> Char -> Integer
        -- Returns implementation-dependent garbage for non-decimal digits.
        value n c = fromIntegral (ord c - ord '0') * 10 ^ n

{- | Parse a number sign.

=== __Examples:__

>>> parse sign (initialize "+123")
Right (Positive,Counter 1 "123")

>>> parse sign (initialize "-123")
Right (Negative,Counter 1 "123")

>>> parse sign (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])

>>> parse sign (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE sign #-}
sign
    :: (HasOffset s, ElementOf s ~ Char)
    => Parser s (ParseError (ValidationError Char)) Sign
sign = first (fmap (either id absurd)) $ validate v one
    where
        v x = case x of
            y | y == '-' -> Right Negative
            y | y == '+' -> Right Positive
            _            -> Left $ pure x

{- | Parse a signed 'Integer' in decimal format. -}
{-# INLINE integer #-}
integer
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ Char, ElementOf (PrefixOf s) ~ Char)
    => Parser s (ParseError (ValidationError (ElementOf s))) Integer
integer = do
    sgn <- first absurd (fromMaybe Positive <$> Combinators.maybe sign)
    number <- positive
    case sgn of
        Positive -> pure number
        Negative -> pure (-number)


{- | Parse a single (unicode) letter. -}
{-# INLINE letter #-}
letter
    :: (HasOffset s, ElementOf s ~ Char)
    => Parser s (ParseError (ValidationError Char)) Char
letter = satisfy isLetter

{- | Parse a word.

=== __Examples:__

>>> parse word (initialize "abc  ")
Right ("abc",Counter 3 "  ")

>>> parse word (initialize "abc__  ")
Right ("abc",Counter 3 "__  ")

>>> parse word (initialize "__abc")
Left (Cons (ErrorItem 1 (ValidationError '_')) [])

>>> parse word (initialize "  __abc")
Left (Cons (ErrorItem 1 (ValidationError ' ')) [])
-}
{-# INLINE word #-}
word
    :: (HasOffset s, Splittable s, ElementOf s ~ Char)
    => Parser s (ParseError (ValidationError Char)) (PrefixOf s)
word = takeWith1 isLetter

{- | Parse an identifier.

An identifier is a letter followed by any combination of letters, digits and the characters @\'-\'@
or @\'_\'@.

=== __Examples:__

>>> parse identifier (initialize "abc  ")
Right ("abc",Counter 3 "  ")

>>> parse identifier (initialize "abc__  ")
Right ("abc__",Counter 5 "  ")

>>> parse identifier (initialize "__abc")
Left (Cons (ErrorItem 1 (ValidationError '_')) [])

>>> parse identifier (initialize "  __abc")
Left (Cons (ErrorItem 1 (ValidationError ' ')) [])
-}
{-# INLINE identifier #-}
identifier
    :: (HasOffset s, Splittable s, ElementOf s ~ Char)
    => Parser s (ParseError (ValidationError Char)) (PrefixOf s)
identifier = do
        x <- first absurd $ lookAhead letter
        case x of
            Left e  -> throw e
            Right _ -> first absurd $ takeWith v
    where
        v :: Char -> Bool
        v c = isLetter c || isDigit c || '-' == c || '_' == c

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

>>> parse escape (initialize "\\n")
Right ("\n",Counter 2 "")

>>> parse escape (initialize "\\s")
Right (" ",Counter 2 "")

>>> parse escape (initialize "\\\\")
Right ("\\",Counter 2 "")

>>> parse escape (initialize "s")
Left (Cons (ErrorItem 1 (EscapeCharError 's')) [])

>>> parse escape (initialize "\\a")
Left (Cons (ErrorItem 2 (EscapeSequenceError 'a')) [])

>>> parse escape (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE escape #-}
escape
    :: forall s . (HasOffset s, Splittable s, ElementOf s ~ Char)
    => Parser s (ParseError EscapeError) (PrefixOf s)
escape = do
    c <- first (fmap absurd) one
    if '\\' /= c
        then throwParseError (EscapeCharError c)
        else first (fmap (either id absurd)) (validate v one)
    where
        v :: Char -> EscapeError :+: PrefixOf s
        v c = case c of
            't'  -> Right (single @s '\t')
            'n'  -> Right (single @s '\n')
            'v'  -> Right (single @s '\v')
            'f'  -> Right (single @s '\f')
            'r'  -> Right (single @s '\r')
            's'  -> Right (single @s ' ')
            '\'' -> Right (single @s '\'')
            '"'  -> Right (single @s '"')
            '\\' -> Right (single @s '\\')
            _    -> Left (EscapeSequenceError c)

{- | Parse a quoted string with escape sequences.

The quote characters are @\'\\\'\'@ and @\'\"\'@. The available escape sequences are the ones
accepted by the 'escape' parser.

note(s):

  * A quoted string does /not/ span multiple lines.

=== __Examples:__

The examples are run in ghci so the escape character @\'\\\'@ must itself be escaped.

>>> parse string (initialize "'quoted string'")
Right ("quoted string",Counter 15 "")

>>> parse string (initialize "'Quoted string with many \\s\\s\\s spaces.'")
Right ("Quoted string with many     spaces.",Counter 40 "")

>>> parse string (initialize "'Quoted string with many\\s\\s\\sspaces and one \\\'\\\\\\\' escape character.'")
Right ("Quoted string with many   spaces and one '\\' escape character.",Counter 70 "")
-}
{-# INLINEABLE string #-}
string
    :: forall s . (HasOffset s, Splittable s, ElementOf s ~ Char, Monoid (PrefixOf s))
    => Parser s (ParseError StringError) (PrefixOf s)
string = do
        c <- onParseError StringError startQuote
        blocks <- manyTill (onParseError StringError (endQuote c)) (esc <|> block c)
        pure $ foldl' (<>) mempty blocks
    where
        startQuote :: Parser s (ParseError QuoteError) Char
        startQuote = do
            c <- first (fmap absurd) one
            if '\'' == c || '\"' == c then pure c else throwParseError (StartQuoteError c)

        endQuote :: Char -> Parser s (ParseError QuoteError) Char
        endQuote q = do
            c <- first (fmap absurd) one
            if q == c || q == c then pure c else throwParseError (EndQuoteError c)

        esc :: Parser s (ParseError StringError) (PrefixOf s)
        esc = onParseError StringError escape

        block :: Char -> Parser s (ParseError StringError) (PrefixOf s)
        block q = onParseError StringError $ takeWith1 (\ c -> '\\' /= c && '\n' /= c && c /= q)

{- | Parse a line comment.

A line comment starts with @p@ and runs to the end of the line (character @'\\n'@), returning the
prefix in-between.

note(s):

    * If the streamable contains Windows end of lines, then the comment text will contain an ending
    @'\\r'@. This can only be stripped by assuming more about @'PrefixOf' s@ (the practical
    solution) or complicating the implementation.
-}
comment
    :: (HasOffset s, Splittable s, ElementOf s ~ Char)
    => Parser s e ()                    -- ^ Parser for start of line comment.
    -> Parser s e (PrefixOf s)
comment p = p *> first absurd (takeWith (/= '\n') <* Combinators.maybe cr)
