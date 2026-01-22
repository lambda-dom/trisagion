{- |
Module: Trisagion.Typeclasses.Splittable

The @Splittable@ typeclass.
-}

module Trisagion.Typeclasses.Splittable (
    -- * Typeclasses.
    Splittable (..),
) where

-- Imports.
import qualified Data.List as List (splitAt, span)

-- Package.
import qualified Trisagion.Utils.List as List (splitAtExact, matchPrefix)
import Trisagion.Typeclasses.Streamable (Streamable)


{- | The @Splittable@ typeclass. -}
class Streamable m a s => Splittable m a b s | s -> b where
    {-# MINIMAL splitAtM, spanM, singleton, splitAtExactM, matchM #-}

    {- | Split a fixed size prefix from the stream. -}
    splitAtM :: Word -> s -> m (b, s)

    {- | Split the longest prefix from the stream whose elements satisfy a predicate. -}
    spanM :: (a -> Bool) -> s -> m (b, s)

    {- | Pure function to convert a stream element to a prefix. -}
    singleton :: forall n -> forall t -> (s ~ t, m ~ n) => a -> b

    {- | Split a prefix of exact size. -}
    splitAtExactM :: Word -> s -> m (Maybe (b, s))

    {- | Parse and drop a matching prefix. -}
    matchM :: b -> s -> m (Maybe s)


-- Instances.
instance (Eq a, Monad m) => Splittable m a [a] [a] where
    {-# INLINE splitAtM #-}
    splitAtM :: Word -> [a] -> m ([a], [a])
    splitAtM n = pure . List.splitAt (fromIntegral n)

    {-# INLINE spanM #-}
    spanM :: (a -> Bool) -> [a] -> m ([a], [a])
    spanM p = pure . List.span p

    {-# INLINE splitAtExactM #-}
    splitAtExactM :: Word -> [a] -> m (Maybe ([a], [a]))
    splitAtExactM n = pure . List.splitAtExact n

    {-# INLINE matchM #-}
    matchM :: [a] -> [a] -> m (Maybe [a])
    matchM xs = pure . List.matchPrefix xs

    {-# INLINE singleton #-}
    singleton _ _ x = [x]
