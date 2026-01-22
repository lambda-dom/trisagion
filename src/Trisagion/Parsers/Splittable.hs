{- |
Module: Trisagion.Parsers.Splittable

Parsers with @'Splittable' m a b s@ constraints.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers @'Splittable' m a b s => 'ParserT' m s e a@.
    take,
    takeWhile,
    drop,
    dropWhile,
    takeExact,
    match,
    takeWhile1,
    isolate,
    consumed,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (take, takeWhile, drop, dropWhile)

-- Base.
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.ParserT (ParserT, mapError, lookAhead, throw, parse, eval)
import Trisagion.Parsers.Combinators (skip)
import Trisagion.Parsers.HasOffset (offset)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), satisfy)


{- | Parse a fixed size prefix from the stream. -}
{-# INLINE take #-}
take :: Splittable m a b s => Word -> ParserT s Void m b
take n = do
    (xs, ys) <- get >>= lift . splitAtM n
    put ys $> xs

{- | Parse the longest prefix from the stream whose elements satisfy a predicate. -}
{-# INLINE takeWhile #-}
takeWhile :: Splittable m a b s => (a -> Bool) -> ParserT s Void m b
takeWhile p = do
    (xs, ys) <- get >>= lift . spanM p
    put ys $> xs

{- | Drop a fixed size prefix from the stream. -}
drop :: Splittable m a b s => Word -> ParserT s Void m ()
drop = skip . take

{- | Drop the longest prefix from the stream whose elements satisfy a predicate. -}
dropWhile :: Splittable m a b s => (a -> Bool) -> ParserT s Void m ()
dropWhile = skip . takeWhile

{- | Parse the longest prefix with at least one element, whose elements satisfy a predicate. -}
takeWhile1 :: Splittable m a b s => (a -> Bool) -> ParserT s (ValidationError a :+: InputError) m b
takeWhile1 p = do
    x <- mapError absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throw e
        Right _ -> mapError absurd $ takeWhile p

{- | Parse an exact, fixed size prefix from the stream. -}
{-# INLINE takeExact #-}
takeExact :: Splittable m a b s => Word -> ParserT s InputError m b
takeExact n = do
    r <- get >>= lift . splitAtExactM n
    case r of
        Just (xs, ys) -> put ys $> xs
        Nothing       -> throw $ InputError n

{- | Parse a matching prefix from the stream. -}
{-# INLINE match #-}
match :: Splittable m a b s => b -> ParserT s (ValidationError b) m ()
match xs = do
    r <- get >>= lift . matchM xs
    case r of
        Just ys -> put ys $> ()
        Nothing -> throw $ ValidationError xs

{- | Run a parser isolated to a fixed size prefix of the stream.

The prefix on which the parser runs may have a size smaller than @n@ if there is not enough input
in the stream. Any unconsumed input in the prefix is returned along with the result.
-}
{-# INLINE isolate #-}
isolate
    :: Splittable m a b s
    => Word                             -- ^ Prefix size.
    -> ParserT b e m a                  -- ^ Parser to run on the prefix.
    -> ParserT s e m (a, b)
isolate n p = do
    prefix <- mapError absurd $ take n
    r <- lift (parse p prefix)
    case r of
        Left e  -> throw e
        Right x -> pure x

{- | Run the parser and return its result along with the prefix of consumed input.

note(s):

  * Implementation requires computing the difference of offsets, so it implicitly relies on
    normality of @p@.
-}
{-# INLINE consumed #-}
consumed
    :: (HasOffset m s, Splittable m a b s)
    => ParserT s e m a                  -- ^ Parser to run.
    -> ParserT s e m (b, a)
consumed p = do
    xs    <- get
    start <- mapError absurd offset
    x     <- p
    end   <- mapError absurd offset
    -- Implicitly relies on the parser @p@ being normal, for positivity of @end - start@.
    ys    <- either absurd id <$> lift (eval (take (end - start)) xs)
    pure (ys, x)
