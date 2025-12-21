{- |
Module: Trisagion.Parsers.Splittable

Parsers with @Splittable m a b s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers.
    take,
    drop,
    takeWith,
    dropWith,
    takeExact,
    takeWith1,
    isolate,
    match,
    consumed,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (take, drop)

-- Base.
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (MonadTrans (..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.ParserT (ParserT, parse, lookAhead, throw, mapError)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), satisfy)


{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.
-}
{-# INLINE take #-}
take :: Splittable m a b s => Word -> ParserT s Void m b
take n = do
    (prefix, remainder) <- get >>= \xs ->  lift (splitAtM n xs)
    put remainder $> prefix

{- | Drop a fixed size prefix from the stream. -}
{-# INLINE drop #-}
drop :: Splittable m a b s => Word -> ParserT s Void m ()
drop n = do
    (_, remainder) <- get >>= \xs ->  lift (splitAtM n xs)
    put remainder $> ()

{- | Parse the longest prefix whose elements satisfy a predicate. -}
{-# INLINE takeWith #-}
takeWith :: Splittable m a b s => (a -> Bool) -> ParserT s Void m b
takeWith p = do
    (prefix, remainder) <- get >>= \xs ->  lift (splitWithM p xs)
    put remainder $> prefix

{- | Parse the longest prefix whose elements satisfy a predicate. -}
{-# INLINE dropWith #-}
dropWith :: Splittable m a b s => (a -> Bool) -> ParserT s Void m ()
dropWith p = do
    (_, remainder) <- get >>= \xs ->  lift (splitWithM p xs)
    put remainder $> ()

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix.
-}
{-# INLINE takeExact #-}
takeExact
    :: (HasOffset m s, Splittable m a b s, MonoFoldable a b)
    => Word -> ParserT s (ParseError InputError) m b
takeExact n = do
    m <- get >>= \ xs -> lift (offset xs)
    prefix <- mapError absurd $ take n
    if monolength prefix /= n
        then throw $ ParseError m (InputError n)
        else pure prefix

{- | Parse a matching prefix.

note(s):

    * Implementation requires computing the length of the argument prefix.
-}
{-# INLINE match #-}
match
    :: (HasOffset m s, Splittable m a b s, Eq b, MonoFoldable a b)
    => b                                -- ^ Matching prefix.
    -> ParserT s (ParseError ((ValidationError b) :+: InputError)) m b
match xs = do
    m <- get >>= \ ys -> lift (offset ys)
    prefix <- mapError (fmap Right) $ takeExact (monolength xs)
    if xs == prefix
        then pure xs
        else throw $ ParseError m (Left (ValidationError xs))

{- | Parse the longest prefix with at least one element, whose elements satisfy a predicate. -}
{-# INLINE takeWith1 #-}
takeWith1
    :: (Splittable m a b s, HasOffset m s)
    => (a -> Bool)                      -- ^ Predicate on @a@.
    -> ParserT s (ParseError (ValidationError a :+: InputError)) m b
takeWith1 p = do
    x <- mapError absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throw e
        Right _ -> mapError absurd $ takeWith p

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
consumed :: (HasOffset m s, Splittable m a b s) => ParserT s e m a -> ParserT s e m (b, a)
consumed p = do
    xs <- get
    start <- lift (offset xs)
    x  <- p
    end <- get >>= \ ys -> lift (offset ys)
    -- Implicitly relies on the parser @p@ being normal, for positivity of @end - start@.
    ys <- mapError absurd $ lift (fst <$> splitAtM (end - start) xs)
    pure (ys, x)
