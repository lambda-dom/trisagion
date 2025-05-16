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
    digit,
    positive,
    sign,
    integer,

    -- * Alphanumeric.
    letter,
    word,
    identifier,
    quote,
    escape,

    -- * Other lexemes.
    comment,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isSpace, isDigit, ord, isLetter)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor(..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser, takeWith, one)
import Trisagion.Parsers.Combinators (lookAhead)
import qualified Trisagion.Parsers.Combinators as Combinators (maybe)
import Trisagion.Parsers.ParseError (ValidationError, validate)
import Trisagion.Parsers.Streamable (matchElem, satisfy)
import Trisagion.Parsers.Splittable (takeWith1)


{- | The sign of a number. -}
data Sign = Negative | Positive
    deriving stock (Eq, Ord, Bounded, Enum, Show)


{- | Enumerate the elements of a list downwards.

The resulting list has at most @n + 1@ elements.
-}
{-# INLINE enumDown #-}
enumDown :: Word -> [a] -> [(Word, a)]
enumDown n = zip ns
    where
        ns = if n == 0 then [0] else [n, pred n .. 0]

{- | Parse a line feed (character @'\\n'@). -}
{-# INLINE lf #-}
lf
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
lf = matchElem '\n'

{- | Parse a carriage return (character @'\\r'@). -}
{-# INLINE cr #-}
cr
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
cr = matchElem '\r'


{- | Parse a, possibly null, prefix of whitespace. -}
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
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
digit = satisfy isDigit

{- | Parse a positive 'Integer' in decimal format. -}
{-# INLINEABLE positive #-}
positive
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ Char, ElementOf (PrefixOf s) ~ Char)
    => Parser s (ParseError s (ValidationError (ElementOf s))) Integer
positive = do
        digits <- takeWith1 isDigit
        let xs = enumDown (pred (monolength digits)) (monotoList digits)
        pure $ foldl' (+) 0 $ uncurry value <$> xs
    where
        value :: Word -> Char -> Integer
        -- Returns implementation-dependent garbage for non-decimal digits.
        value n c = fromIntegral (ord c - ord '0') * 10 ^ n

{- | Parse a number sign. -}
{-# INLINE sign #-}
sign
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Sign
sign = first (fmap (either id absurd)) $ validate v one
    where
        v x = case x of
            y | y == '-' -> Right Negative
            y | y == '+' -> Right Positive
            _            -> Left $ pure x

{- | Parse a signed 'Integer' in decimal format. -}
{-# INLINE integer #-}
integer
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ Char, ElementOf (PrefixOf s) ~ Char)
    => Parser s (ParseError s (ValidationError (ElementOf s))) Integer
integer = do
    sgn <- first absurd (fromMaybe Positive <$> Combinators.maybe sign)
    number <- positive
    case sgn of
        Positive -> pure number
        Negative -> pure (-number)


{- | Parse a single (unicode) letter. -}
{-# INLINE letter #-}
letter
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
letter = satisfy isLetter

{- | Parse a word. -}
{-# INLINE word #-}
word
    :: (Splittable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) (PrefixOf s)
word = takeWith1 isLetter

{- | Parse an identifier.

An identifier is a letter followed by any combination of letters, digits and the characters @-@
or @_@.-}
{-# INLINE identifier #-}
identifier
    :: (Splittable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) (PrefixOf s)
identifier = do
        x <- first absurd $ lookAhead letter
        case x of
            Left e  -> throwError e
            Right _ -> first absurd $ takeWith v
    where
        v :: Char -> Bool
        v c = isLetter c || isDigit c || '-' == c || '_' == c

{- | Parse a string quote character, either @\'@ or @\"@. -}
{-# INLINE quote #-}
quote
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
quote = satisfy (\ c -> '\'' == c || '\"' == c)

{- | Parse an escape sequence inside a quoted string.

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
| @\\\\@   | 92  | character @\\@            |
+----------+-----+---------------------------+
-}
{-# INLINE escape #-}
escape
    :: (Streamable s, ElementOf s ~ Char)
    => Parser s (ParseError s (ValidationError Char)) Char
escape = matchElem '\\' *> first (fmap (either id absurd)) (validate v one)
    where
        v c = case c of
            't'  -> pure '\t'
            'n'  -> pure '\n'
            'v'  -> pure '\v'
            'f'  -> pure '\f'
            'r'  -> pure '\r'
            's'  -> pure ' '
            '\'' -> pure '\''
            '"'  -> pure '"'
            _    -> throwError $ pure c


{- | Parse a line comment.

A line comment starts with @p@ and runs to the end of the line (character @'\\n'@), returning the
prefix in-between.

note(s):

    * If the streamable contains Windows end of lines, then the comment text will contain an ending
    @'\\r'@. This can only be stripped by assuming more about @'PrefixOf' s@ (the practical
    solution) or complicating the implementation.
-}
comment
    :: (Splittable s, ElementOf s ~ Char)
    => Parser s e ()                    -- ^ Parser for start of line comment.
    -> Parser s e (PrefixOf s)
comment p = p *> first absurd (takeWith (/= '\n') <* Combinators.maybe cr)
