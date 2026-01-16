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
import Data.List (uncons, singleton)

-- Non-hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))


{- | The @Streamable@ typeclass of input streams. -}
class (Monad m, MonoFunctor a s) => Streamable m a s | s -> a where
    {-# MINIMAL unconsM #-}

    {- | Get the first element from the stream. -}
    unconsM :: s -> m (Maybe (a, s))

    {- | Convert a stream to a list. -}
    toListM :: s -> m [a]
    toListM xs = do
        x <- unconsM xs
        case x of
            Nothing      -> pure []
            Just (y, ys) -> (y :) <$> toListM ys

    {- | Monadic check for the end of the input stream. -}
    nullM :: s -> m Bool
    nullM = fmap (maybe True (const False)) . unconsM


-- Instances.
instance Monad m => Streamable m a (Maybe a) where
    {-# INLINE unconsM #-}
    unconsM :: Maybe a -> m (Maybe (a, Maybe a))
    unconsM xs = case xs of
        Nothing -> pure $ Nothing
        Just x  -> pure $ Just (x, Nothing)

    {-# INLINE toListM #-}
    toListM :: Maybe a -> m [a]
    toListM = pure . maybe [] singleton

    {-# INLINE nullM #-}
    nullM :: Maybe a -> m Bool
    nullM = pure . isNothing

instance Monad m => Streamable m a [a] where
    {-# INLINE unconsM #-}
    unconsM :: [a] -> m (Maybe (a, [a]))
    unconsM = pure . uncons

    {-# INLINE toListM #-}
    toListM :: [a] -> m [a]
    toListM = pure

    {-# INLINE nullM #-}
    nullM :: [a] -> m Bool
    nullM = pure . null
