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
import qualified Data.List as List (uncons)

-- Non-hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor)


{- | The @Streamable@ typeclass of input streams. -}
class MonoFunctor a s => Streamable m a s | s -> a where
    {-# MINIMAL unconsM #-}

    {- | Uncons the first element from the stream. -}
    unconsM :: s -> m (Maybe (a, s))


-- Instances.
instance Monad m => Streamable m a (Maybe a) where
    {-# INLINE unconsM #-}
    unconsM :: Maybe a -> m (Maybe (a, Maybe a))
    unconsM Nothing  = pure $ Nothing
    unconsM (Just x) = pure $ Just (x, Nothing)

instance Monad m => Streamable m a [a] where
    {-# INLINE unconsM #-}
    unconsM :: [a] -> m (Maybe (a, [a]))
    unconsM = pure . List.uncons
