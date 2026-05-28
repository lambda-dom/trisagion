{- |
Module: Trisagion.Parsers.Splittable

Parsers with @'Splittable' a b s@ constraints.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers @'Splittable' a b s => 'Parser' s e a@.
    takePrefix,
    takeWith,
    skipPrefix,
    skipWith,
    takeWith1,
    takeExact,
    matchPrefix,
    isolate,
    consumed,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (splitAt, span)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..), gets)

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Splittable (Splittable (splitAt, span, splitAtExact) )
import Trisagion.Typeclasses.HasOffset (HasOffset)
import qualified Trisagion.Typeclasses.Splittable as Splittable (Splittable (match))
import Trisagion.Parser (Parser, lookAhead, parse, eval)
import Trisagion.Parsers.Combinators (skip)
import Trisagion.Parsers.Streamable (InputError (..), ValidationError (..), satisfy)
import Trisagion.Parsers.HasOffset (offset)
import Control.Monad.Except (MonadError(..))


-- $setup
-- >>> import Trisagion.Splittable
-- >>> import Trisagion.Parser


{- | Parse a fixed size prefix from the stream.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.

=== __Examples:__

>>> parse (takePrefix 2) "0123"
Right ("01","23")

>>> parse (takePrefix 10) "0123"
Right ("0123","")
-}
{-# INLINE takePrefix #-}
takePrefix :: Splittable a b s => Word -> Parser s Void b
takePrefix n = do
    (xs, ys) <- gets $ splitAt n
    put ys $> xs

{- | Parse the longest prefix from the stream whose elements satisfy a predicate.

=== __Examples:__

>>> parse (takeWith ('3' /=)) "0123"
Right ("012","3")

>>> parse (takeWith ('3' /=)) ""
Right ("","")
-}
{-# INLINE takeWith #-}
takeWith :: Splittable a b s => (a -> Bool) -> Parser s Void b
takeWith p = do
    (xs, ys) <- gets $ span p
    put ys $> xs

{- | Skip a fixed size prefix from the stream.

=== __Examples:__

>>> parse (skipPrefix 2) "0123"
Right ((),"23")

>>> parse (skipPrefix 10) "0123"
Right ((),"")
-}
{-# INLINE skipPrefix #-}
skipPrefix :: Splittable a b s => Word -> Parser s Void ()
skipPrefix = skip . takePrefix

{- | Skip the longest prefix from the stream whose elements satisfy a predicate.

=== __Examples:__

>>> parse (skipWith ('3' /=)) "0123"

>>> parse (skipWith ('3' /=)) ""
 -}
{-# INLINE skipWith #-}
skipWith :: Splittable a b s => (a -> Bool) -> Parser s Void ()
skipWith = skip . takeWith

{- | Parse the longest prefix with at least one element, whose elements satisfy a predicate.

=== __Examples:__

>>> parse (takeWith1 ('0' ==)) "0123"

>>> parse (takeWith1 ('0' ==)) "0003"

>>> parse (takeWith1 ('1' ==)) "0123"

>>> parse (takeWith1 ('0' ==)) ""
-}
{-# INLINE takeWith1 #-}
takeWith1 :: Splittable a b s => (a -> Bool) -> Parser s (ValidationError a :+: InputError) b
takeWith1 p = do
    x <- first absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throwError e
        Right _ -> first absurd $ takeWith p

{- | Parse an exact, fixed size prefix from the stream.

=== __Examples:__

>>> parse (takeExact 2) "0123"

>>> parse (takeExact 10) "0123"
-}
{-# INLINE takeExact #-}
takeExact :: Splittable a b s => Word -> Parser s InputError b
takeExact n = do
    r <- gets $ splitAtExact n
    case r of
        Just (xs, ys) -> put ys $> xs
        Nothing       -> throwError $ InputError n

{- | Parse a matching prefix from the stream.

=== __Examples:__

>>> parse (match "01") "0123"

>>> parse (match "012345") "0123"

>>> parse (match "{}") "0123"
-}
{-# INLINE matchPrefix #-}
matchPrefix :: Splittable a b s => b -> Parser s (ValidationError b) ()
matchPrefix xs = do
    r <- gets $ Splittable.match xs
    case r of
        Just ys -> put ys $> ()
        Nothing -> throwError $ ValidationError xs

{- | Run a parser isolated to a fixed size prefix of the stream.

The prefix on which the parser runs may have a size smaller than @n@ if there is not enough input
in the stream. Any unconsumed input in the prefix is returned along with the result.

=== __Examples:__

>>> parse (isolate 2 one) "0123"

>>> parse (isolate 2 one) "0"

>>> parse (isolate 2 one) ""
-}
{-# INLINE isolate #-}
isolate
    :: Splittable a b s
    => Word                             -- ^ Prefix size.
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

>>> parse (consumed one) "0123"

>>> parse (consumed one) ""
-}
{-# INLINE consumed #-}
consumed
    :: (HasOffset s, Splittable a b s)
    => Parser s e a                     -- ^ Parser to run.
    -> Parser s e (b, a)
consumed p = do
    xs    <- get
    start <- first absurd offset
    x     <- p
    end   <- first absurd offset
    -- Implicitly relies on the parser @p@ being normal, for positivity of @end - start@.
    let ys = either absurd id $ eval (takePrefix (end - start)) xs
    pure (ys, x)
