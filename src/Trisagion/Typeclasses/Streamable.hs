{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),
) where

-- Imports.
-- Non-hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))


{- | The @Streamable@ typeclass of input streams. -}
class (Monad m, MonoFunctor a s) => Streamable m a s | s -> a where
    {-# MINIMAL unconsM #-}

    {- | Get the first element from the stream. -}
    unconsM :: s -> m (Maybe (a, s))

    {- | Monadic check for the end of the input stream. -}
    {-# INLINE eoi #-}
    eoi :: s -> m Bool
    eoi xs = do
        r <- unconsM xs
        case r of
            Nothing -> pure True
            _       -> pure False
