{- |
Module: Trisagion.Parsers.Split

Parsers with @'Split' a b s@ constraints on the input stream.
-}

module Trisagion.Parsers.Split (
    -- * Parsers @'Split' a b s => 'Parser' s e b@.
    takeN,
    takeWith,
    skipN,
    skipWith,
    takeRemainder,
    takeWith1,
    takeExact,
    skipExact,
    matchPrefix,
    isolate,
    consumed,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.HasOffset (HasOffset)
import Trisagion.Split (
    Split,
    splitRemainder,
    splitN,
    splitWith,
    splitExact,
    dropN,
    dropWith,
    dropExact)
import qualified Trisagion.Split as Split (matchPrefix)
import Trisagion.Parser (Parser, parse, eval, get, put, throw)
import Trisagion.Parsers.Combinators (lookAhead)
import Trisagion.Parsers.IStream (InputError (..), ValidationError (..), satisfy)
import Trisagion.Parsers.HasOffset (offset)


-- $setup
-- >>> import Trisagion.Streams.Counter
-- >>> import Trisagion.Parser
-- >>> import Trisagion.Parsers.Source


{- | Parse a fixed size prefix from the input stream.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.

=== __Examples:__

>>> parse (takeN 2) "0123"
Right ("01","23")

>>> parse (takeN 10) "0123"
Right ("0123","")
-}
{-# INLINE takeN #-}
takeN :: Split a b s => Int -> Parser s Void b
takeN n = do
    (xs, ys) <- fmap (splitN n) get
    put ys $> xs

{- | Parse the longest prefix from the input stream whose elements satisfy a predicate.

=== __Examples:__

>>> parse (takeWith ('3' /=)) "0123"
Right ("012","3")

>>> parse (takeWith ('3' /=)) ""
Right ("","")
-}
{-# INLINE takeWith #-}
takeWith :: Split a b s => (a -> Bool) -> Parser s Void b
takeWith p = do
    (xs, ys) <- fmap (splitWith p) get
    put ys $> xs

{- | Skip a fixed size prefix from the input stream.

=== __Examples:__

>>> parse (skipN 2) "0123"
Right ((),"23")

>>> parse (skipN 10) "0123"
Right ((),"")
-}
{-# INLINE skipN #-}
skipN :: Split a b s => Int -> Parser s Void ()
skipN n = fmap (dropN n) get >>= put

{- | Skip the longest prefix from the input stream whose elements satisfy a predicate.

=== __Examples:__

>>> parse (skipWith ('3' /=)) "0123"
Right ((),"3")

>>> parse (skipWith ('3' /=)) ""
Right ((),"")
 -}
{-# INLINE skipWith #-}
skipWith :: Split a b s => (a -> Bool) -> Parser s Void ()
skipWith p = fmap (dropWith p) get >>= put

{- | Parse the remainder of the input stream as a prefix. -}
{-# INLINE takeRemainder #-}
takeRemainder :: Split a b s => Parser s Void b
takeRemainder = do
    (prefix, rest) <- fmap splitRemainder get
    put rest $> prefix

{- | Parse the longest prefix with at least one element, whose elements satisfy a predicate.

=== __Examples:__

>>> parse (takeWith1 ('0' ==)) "0123"
Right ("0","123")

>>> parse (takeWith1 ('0' ==)) "0003"
Right ("000","3")

>>> parse (takeWith1 ('1' ==)) "0123"
Left (Left (ValidationError '0'))

>>> parse (takeWith1 ('0' ==)) ""
Left (Right (InputError 1))
-}
{-# INLINE takeWith1 #-}
takeWith1 :: Split a b s => (a -> Bool) -> Parser s (InputError :+: ValidationError a) b
takeWith1 p = do
    x <- first absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throw e
        Right _ -> first absurd $ takeWith p

{- | Parse an exact, fixed size prefix from the input stream.

=== __Examples:__

>>> parse (takeExact 2) "0123"
Right ("01","23")

>>> parse (takeExact 10) "0123"
Left (InputError 10)
-}
{-# INLINE takeExact #-}
takeExact :: Split a b s => Int -> Parser s InputError b
takeExact n = do
    r <- fmap (splitExact n) get
    case r of
        Just (xs, ys) -> put ys $> xs
        Nothing       -> throw $ InputError n

{- | Skip an exact, fixed size prefix from the input stream.

=== __Examples:__

>>> parse (skipExact 2) "0123"
Right ((),"23")

>>> parse (skipExact 10) "0123"
Left (InputError 10)
-}
{-# INLINE skipExact #-}
skipExact :: Split a b s => Int -> Parser s InputError ()
skipExact n = do
    xs <- fmap (dropExact n) get
    case xs of
        Just ys -> put ys $> ()
        Nothing -> throw $ InputError n

{- | Parse a matching prefix from the input stream.

=== __Examples:__

>>> parse (matchPrefix "01") "0123"
Right ((),"23")

>>> parse (matchPrefix "012345") "0123"
Left (ValidationError "012345")

>>> parse (matchPrefix "{}") "0123"
Left (ValidationError "{}")
-}
{-# INLINE matchPrefix #-}
matchPrefix :: Split a b s => b -> Parser s (ValidationError b) ()
matchPrefix xs = do
    r <- fmap (Split.matchPrefix xs) get
    case r of
        Just ys -> put ys $> ()
        Nothing -> throw $ ValidationError xs

{- | Run a parser isolated to a fixed size prefix of the input stream.

The prefix on which the parser runs may have a size smaller than @n@ if there is not enough input
in the stream. Any unconsumed input in the prefix is returned along with the result.

=== __Examples:__

>>> parse (isolate 2 one) "0123"
Right (('0',"1"),"23")

>>> parse (isolate 2 one) "0"
Right (('0',""),"")

>>> parse (isolate 2 one) ""
Left (InputError 1)

>>> parse (isolate 2 (matchOne '1')) "0123"
Left (Left (ValidationError '0'))
-}
{-# INLINE isolate #-}
isolate
    :: Split a b s
    => Int                              -- ^ Prefix size.
    -> Parser b e a                     -- ^ Parser to run on the prefix.
    -> Parser s e (a, b)
isolate n p = do
    prefix <- first absurd $ takeN n
    either throw pure $ parse p prefix

{- | Run the parser and return its result along with the prefix of consumed input.

note(s):

  * Implementation requires computing the difference of offsets, so it implicitly relies on
    @p@ being monotonic.

=== __Examples:__

>>> parse (consumed one) (initialize "0123")
Right (('0',"0"),Counter 1 "123")

>>> parse (consumed one) (initialize "")
Left (InputError 1)
-}
{-# INLINE consumed #-}
consumed
    :: (HasOffset s, Split a b s)
    => Parser s e a                     -- ^ Parser to run.
    -> Parser s e (a, b)
consumed p = do
    xs    <- get
    start <- first absurd offset
    x     <- p
    end   <- first absurd offset
    -- Implicitly relies on the parser @p@ being monotonic, for positivity of @end - start@.
    let ys = either absurd id $ eval (takeN (end - start)) xs
    pure (x, ys)
