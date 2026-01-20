{- |
Module: Trisagion.Typeclasses.Splittable

The @Splittable@ typeclass.
-}

module Trisagion.Typeclasses.Splittable (
    -- * Typeclasses.
    Splittable (..),

    -- * Parsers with @'Splittable' m a b s@ constraints,
    isolate,
    consumed,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (take, drop, takeWhile, dropWhile)

-- Base.
import Data.Functor (($>))
import Data.Void (Void, absurd)
import qualified Data.List as List (drop, dropWhile, splitAt, span)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Utils.List (splitAtExact, matchPrefix)
import Trisagion.ParserT (ParserT, mapError, lookAhead, throw, parse, eval)
import Trisagion.Parsers.Combinators (skip)
import Trisagion.Typeclasses.Streamable (Streamable (..), ValidationError (..), InputError (..), satisfy)
import Trisagion.Typeclasses.HasOffset (HasOffset (..))


{- | The @Splittable@ typeclass. -}
class Streamable m a s => Splittable m a b s | s -> b where
    {-# MINIMAL take, takeWhile, singleton, takeExact, match #-}

    {- | Parse a fixed size prefix from the stream. -}
    take :: Word -> ParserT s Void m b

    {- | Parse the longest prefix from the stream whose elements satisfy a predicate. -}
    takeWhile :: (a -> Bool) -> ParserT s Void m b

    {- | Pure function to convert a stream element to a prefix. -}
    singleton :: forall n -> forall t -> (s ~ t, m ~ n) => a -> b

    {- | Parse a prefix of exact size. -}
    takeExact :: Word -> ParserT s InputError m b

    {- | Parse a matching prefix. -}
    match :: b -> ParserT s (ValidationError b) m b

    {- | Drop a fixed size prefix from the stream. -}
    drop :: Word -> ParserT s Void m ()
    drop = skip . take

    {- | Drop the longest prefix from the stream whose elements satisfy a predicate. -}
    dropWhile :: (a -> Bool) -> ParserT s Void m ()
    dropWhile = skip . takeWhile

    {- | Parse the longest prefix with at least one element, whose elements satisfy a predicate. -}
    takeWhile1 :: (a -> Bool) -> ParserT s (ValidationError a :+: InputError) m b
    takeWhile1 p = do
        x <- mapError absurd $ lookAhead (satisfy p)
        case x of
            Left e  -> throw e
            Right _ -> mapError absurd $ takeWhile p


-- Instances.
instance (Eq a, Monad m) => Splittable m a [a] [a] where
    {-# INLINE take #-}
    take :: Word -> ParserT [a] Void m [a]
    take n = do
        xs <- get
        let (x, ys) = List.splitAt (fromIntegral n) xs
        put ys $> x

    {-# INLINE takeWhile #-}
    takeWhile :: (a -> Bool) -> ParserT [a] Void m [a]
    takeWhile p = do
        xs <- get
        let (x, ys) = List.span p xs
        put ys $> x

    {-# INLINE takeExact #-}
    takeExact :: Word -> ParserT [a] InputError m [a]
    takeExact n = do
        xs <- get
        case splitAtExact n xs of
            Nothing       -> throw $ InputError n
            Just (ys, zs) -> put zs $> ys

    {-# INLINE match #-}
    match :: [a] -> ParserT [a] (ValidationError [a]) m [a]
    match xs = do
            ys <- get
            case matchPrefix xs ys of
                Nothing -> throw $ ValidationError xs
                Just zs -> put zs $> xs

    {-# INLINE singleton #-}
    singleton _ _ x = [x]

    {-# INLINE drop #-}
    drop :: Word -> ParserT [a] Void m ()
    drop n = do
        xs <- get
        let ys = List.drop (fromIntegral n) xs
        put ys $> ()

    {-# INLINE dropWhile #-}
    dropWhile :: (a -> Bool) -> ParserT [a] Void m ()
    dropWhile p = do
        xs <- get
        let ys = List.dropWhile p xs
        put ys $> ()

    {-# INLINE takeWhile1 #-}
    takeWhile1 :: (a -> Bool) -> ParserT [a] (ValidationError a :+: InputError) m [a]
    takeWhile1 p = do
        xs <- get
        case xs of
            []       -> throw $ Right (InputError 1)
            (x : ys) -> if p x then do
                    let (z, zs) = List.span p ys
                    put zs $> (x : z)
                else throw $ Left (ValidationError x)


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
