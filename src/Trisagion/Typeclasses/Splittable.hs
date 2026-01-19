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

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.ParserT (ParserT, mapError, lookAhead, throw, parse, eval)
import Trisagion.Typeclasses.Streamable (Streamable, ValidationError (..), InputError (..), satisfy)
import Trisagion.Typeclasses.HasOffset (HasOffset (..))


{- | The @Splittable@ typeclass. -}
class Streamable m a s => Splittable m a b s | s -> b where
    {-# MINIMAL take, takeWhile, singleton #-}

    {- | Parse a fixed size prefix from the stream. -}
    take :: Word -> ParserT s Void m b

    {- | Parse the longest prefix from the stream whose elements satisfy a predicate. -}
    takeWhile :: (a -> Bool) -> ParserT s Void m b

    {- | Pure function to convert a stream element to a prefix. -}
    singleton :: forall n -> forall t -> (s ~ t, m ~ n) => a -> b

    {- | Drop a fixed size prefix from the stream. -}
    drop :: Word -> ParserT s Void m ()
    drop n = take n $> ()

    {- | Drop the longest prefix from the stream whose elements satisfy a predicate. -}
    dropWhile :: (a -> Bool) -> ParserT s Void m ()
    dropWhile p = takeWhile p $> ()

    {- | Parse the longest prefix with at least one element, whose elements satisfy a predicate. -}
    takeWhile1 :: (a -> Bool) -> ParserT s (ValidationError a :+: InputError) m b
    takeWhile1 p = do
        x <- mapError absurd $ lookAhead (satisfy p)
        case x of
            Left e  -> throw e
            Right _ -> mapError absurd $ takeWhile p

    {- | Parse an exact, fixed size prefix. -}
    takeExact :: MonoFoldable a b => Word -> ParserT s InputError m b
    takeExact n = do
        prefix <- mapError absurd $ take n
        if monolength prefix /= n
            then throw $ InputError n
            else pure prefix

    {- | Parse a matching prefix. -}
    match :: (Eq b, MonoFoldable a b) => b -> ParserT s (ValidationError b :+: InputError) m b
    match xs = do
        prefix <- mapError Right $ takeExact (monolength xs)
        if xs == prefix
            then pure xs
            else throw $ Left (ValidationError xs)


-- Instances.
-- instance Monad m => Splittable m a [a] [a] where
--     splitAtM :: Word -> [a] -> m ([a], [a])
--     splitAtM n = pure . splitAt (fromIntegral n)

--     splitWithM :: (a -> Bool) -> [a] -> m ([a], [a])
--     splitWithM p = pure . span p

--     singleton _ _ x = [x]


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
