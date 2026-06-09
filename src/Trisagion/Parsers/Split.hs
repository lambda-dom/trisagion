{- |
Module: Trisagion.Parsers.Split

Parsers with @'Split' a b s@ constraints.
-}

module Trisagion.Parsers.Split (
    -- * Parsers @'Split' a b s => 'Parser' s e b@.
    takePrefix,
    takeWith,
    skipPrefix,
    skipWith,
    takeRemainder,
    takeWith1,
    takeExact,
    matchPrefix,
    isolate,
    consumed,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Except (MonadError (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Split (Split, splitRemainder, splitPrefix, splitWith, splitPrefixExact, dropPrefix, dropWith)
import qualified Trisagion.Typeclasses.Split as Split (matchPrefix)
import Trisagion.Parser (Parser, parse, eval)
import Trisagion.Parsers.Combinators (lookAhead)
import Trisagion.Parsers.Source (InputError (..), ValidationError (..), satisfy)
import Trisagion.Parsers.HasOffset (offset)


-- $setup
-- >>> import Trisagion.Streams.Counter
-- >>> import Trisagion.Parser
-- >>> import Trisagion.Parsers.Source


{- | Parse a fixed size prefix from the input stream.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.

=== __Examples:__

>>> parse (takePrefix 2) "0123"
Right ("01","23")

>>> parse (takePrefix 10) "0123"
Right ("0123","")
-}
{-# INLINE takePrefix #-}
takePrefix :: Split a b s => Int -> Parser s Void b
takePrefix n = do
    (xs, ys) <- gets $ splitPrefix n
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
    (xs, ys) <- gets $ splitWith p
    put ys $> xs

{- | Skip a fixed size prefix from the input stream.

=== __Examples:__

>>> parse (skipPrefix 2) "0123"
Right ((),"23")

>>> parse (skipPrefix 10) "0123"
Right ((),"")
-}
{-# INLINE skipPrefix #-}
skipPrefix :: Split a b s => Int -> Parser s Void ()
skipPrefix n = gets (dropPrefix n) >>= put

{- | Skip the longest prefix from the input stream whose elements satisfy a predicate.

=== __Examples:__

>>> parse (skipWith ('3' /=)) "0123"
Right ((),"3")

>>> parse (skipWith ('3' /=)) ""
Right ((),"")
 -}
{-# INLINE skipWith #-}
skipWith :: Split a b s => (a -> Bool) -> Parser s Void ()
skipWith p = gets (dropWith p) >>= put

{- | Parse the remainder of the input stream as a prefix. -}
{-# INLINE takeRemainder #-}
takeRemainder :: Split a b s => Parser s Void b
takeRemainder = do
    (prefix, rest) <- gets splitRemainder
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
takeWith1 :: Split a b s => (a -> Bool) -> Parser s (ValidationError a :+: InputError) b
takeWith1 p = do
    x <- first absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throwError e
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
    r <- gets $ splitPrefixExact n
    case r of
        Just (xs, ys) -> put ys $> xs
        Nothing       -> throwError $ InputError n

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
    r <- gets $ Split.matchPrefix xs
    case r of
        Just ys -> put ys $> ()
        Nothing -> throwError $ ValidationError xs

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
    prefix <- first absurd $ takePrefix n
    case parse p prefix of
        Left e  -> throwError e
        Right x -> pure x

{- | Run the parser and return its result along with the prefix of consumed input.

note(s):

  * Implementation requires computing the difference of offsets, so it implicitly relies on
    normality of @p@.

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
    -- Implicitly relies on the parser @p@ being normal, for positivity of @end - start@.
    let ys = either absurd id $ eval (takePrefix (end - start)) xs
    pure (x, ys)
