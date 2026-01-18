{- |
Module: Trisagion.Typeclasses.Splittable

The @Splittable@ typeclass.
-}

module Trisagion.Typeclasses.Splittable (
    -- * Typeclasses.
    Splittable (..),
) where

-- Imports.
-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | The @Splittable@ typeclass. -}
class Streamable m a s => Splittable m a b s | s -> b where
    {-# MINIMAL splitAtM, splitWithM, singleton #-}

    {- | Split the stream at index @n@ into a pair @(prefix, remainder)@. -}
    splitAtM :: Word -> s -> m (b, s)

    {- | Split the stream with a predicate @a -> Bool@ into a pair @(prefix, remainder)@. -}
    splitWithM :: (a -> Bool) -> s -> m (b, s)

    {- | Pure function to convert a stream element to a prefix. -}
    singleton :: forall n -> forall t -> (s ~ t, n ~ m) => a -> b


-- Instances.
instance Monad m => Splittable m a [a] [a] where
    splitAtM :: Word -> [a] -> m ([a], [a])
    splitAtM n = pure . splitAt (fromIntegral n)

    splitWithM :: (a -> Bool) -> [a] -> m ([a], [a])
    splitWithM p = pure . span p

    singleton _ _ x = [x]
