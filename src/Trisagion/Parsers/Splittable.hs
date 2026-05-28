{- |
Module: Trisagion.Parsers.Splittable

Parsers with @'Splittable' a b s@ constraints.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers @'Splittable' a b s => 'Parser' s e a@.
    take,
    takeWhile,
    drop,
    dropWhile,
    takeWhile1,
    takeExact,
    match,
    isolate,
    consumed,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (take, takeWhile, drop, dropWhile, splitAt, span)

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


{- | Parse a fixed size prefix from the stream. -}
{-# INLINE take #-}
take :: Splittable a b s => Word -> Parser s Void b
take n = do
    (xs, ys) <- gets $ splitAt n
    put ys $> xs

{- | Parse the longest prefix from the stream whose elements satisfy a predicate. -}
{-# INLINE takeWhile #-}
takeWhile :: Splittable a b s => (a -> Bool) -> Parser s Void b
takeWhile p = do
    (xs, ys) <- gets $ span p
    put ys $> xs

{- | Drop a fixed size prefix from the stream. -}
{-# INLINE drop #-}
drop :: Splittable a b s => Word -> Parser s Void ()
drop = skip . take

{- | Drop the longest prefix from the stream whose elements satisfy a predicate. -}
{-# INLINE dropWhile #-}
dropWhile :: Splittable a b s => (a -> Bool) -> Parser s Void ()
dropWhile = skip . takeWhile

{- | Parse the longest prefix with at least one element, whose elements satisfy a predicate. -}
{-# INLINE takeWhile1 #-}
takeWhile1 :: Splittable a b s => (a -> Bool) -> Parser s (ValidationError a :+: InputError) b
takeWhile1 p = do
    x <- first absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throwError e
        Right _ -> first absurd $ takeWhile p

{- | Parse an exact, fixed size prefix from the stream. -}
{-# INLINE takeExact #-}
takeExact :: Splittable a b s => Word -> Parser s InputError b
takeExact n = do
    r <- gets $ splitAtExact n
    case r of
        Just (xs, ys) -> put ys $> xs
        Nothing       -> throwError $ InputError n

{- | Parse a matching prefix from the stream. -}
{-# INLINE match #-}
match :: Splittable a b s => b -> Parser s (ValidationError b) ()
match xs = do
    r <- gets $ Splittable.match xs
    case r of
        Just ys -> put ys $> ()
        Nothing -> throwError $ ValidationError xs

{- | Run a parser isolated to a fixed size prefix of the stream.

The prefix on which the parser runs may have a size smaller than @n@ if there is not enough input
in the stream. Any unconsumed input in the prefix is returned along with the result.
-}
{-# INLINE isolate #-}
isolate
    :: Splittable a b s
    => Word                             -- ^ Prefix size.
    -> Parser b e a                     -- ^ Parser to run on the prefix.
    -> Parser s e (a, b)
isolate n p = do
    prefix <- first absurd $ take n
    case parse p prefix of
        Left e  -> throwError e
        Right x -> pure x

{- | Run the parser and return its result along with the prefix of consumed input.

note(s):

  * Implementation requires computing the difference of offsets, so it implicitly relies on
    normality of @p@.
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
    let ys = either absurd id $ eval (take (end - start)) xs
    pure (ys, x)
