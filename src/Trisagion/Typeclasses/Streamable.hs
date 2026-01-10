{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),
) where

-- Imports.
-- Base.
import Data.Maybe (isNothing)
import Data.List (uncons)

-- Non-hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))


{- | The @Streamable@ typeclass of input streams. -}
class (Functor m, MonoFunctor a s) => Streamable m a s | s -> a where
    {-# MINIMAL unconsM #-}

    {- | Get the first element from the stream. -}
    unconsM :: s -> m (Maybe (a, s))

    {- | Monadic check for the end of the input stream. -}
    {-# INLINE eoi #-}
    eoi :: s -> m Bool
    eoi = fmap (maybe True (const False)) . unconsM


-- Instances.
instance Applicative m => Streamable m a (Maybe a) where
    {-# INLINE unconsM #-}
    unconsM :: Maybe a -> m (Maybe (a, Maybe a))
    unconsM xs = case xs of
        Nothing -> pure $ Nothing
        Just x  -> pure $ Just (x, Nothing)

    {-# INLINE eoi #-}
    eoi :: Maybe a -> m Bool
    eoi = pure . isNothing

instance Applicative m => Streamable m a [a] where
    {-# INLINE unconsM #-}
    unconsM :: [a] -> m (Maybe (a, [a]))
    unconsM = pure . uncons

    {-# INLINE eoi #-}
    eoi :: [a] -> m Bool
    eoi = pure . null
