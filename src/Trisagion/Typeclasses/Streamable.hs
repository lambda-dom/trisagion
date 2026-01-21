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

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor)


{- | The @Streamable@ typeclass of input streams. -}
class (Monad m, MonoFunctor a s) => Streamable m a s | s -> a where
    {-# MINIMAL unconsM #-}

    {- | Get the first element from the stream. -}
    unconsM :: s -> m (Maybe (a, s))

    {- | Monadic check for end of stream. -}
    nullM :: s -> m Bool
    nullM xs = unconsM xs >>= pure . maybe True (const False)


-- Instances.
instance Monad m => Streamable m a (Maybe a) where
    {-# INLINE unconsM #-}
    unconsM :: Maybe a -> m (Maybe (a, Maybe a))
    unconsM xs =
        case xs of
            Just x -> pure $ Just (x, Nothing)
            _      -> pure $ Nothing

    {-# INLINE nullM #-}
    nullM :: Maybe a -> m Bool
    nullM = pure . maybe (True) (const $ False)

instance Monad m => Streamable m a [a] where
    {-# INLINE unconsM #-}
    unconsM :: [a] -> m (Maybe (a, [a]))
    unconsM = pure . List.uncons

    {-# INLINE nullM #-}
    nullM :: [a] -> m Bool
    nullM = pure . null
