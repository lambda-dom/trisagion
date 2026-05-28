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
class Streamable a s => Splittable a b s | s -> b where
    {-# MINIMAL splitAt, span, singleton, splitAtExact, match #-}

    {- | Split a fixed size prefix from the stream. -}
    splitAt :: Word -> s -> (b, s)

    {- | Split the longest prefix from the stream whose elements satisfy a predicate. -}
    span :: (a -> Bool) -> s -> (b, s)

    {- | Pure function to convert a stream element to a prefix. -}
    singleton :: forall t -> s ~ t => a -> b

    {- | Split a prefix of exact size. -}
    splitAtExact :: Word -> s -> Maybe (b, s)

    {- | Parse and drop a matching prefix. -}
    match :: b -> s -> Maybe s


-- Instances.
instance Eq a => Splittable a [a] [a] where
    {-# INLINE splitAt #-}
    splitAt :: Word -> [a] -> ([a], [a])
    splitAt n = List.splitAt (fromIntegral n)

    {-# INLINE span #-}
    span :: (a -> Bool) -> [a] -> ([a], [a])
    span p = List.span p

    {-# INLINE splitAtExact #-}
    splitAtExact :: Word -> [a] -> Maybe ([a], [a])
    splitAtExact n = List.splitAtExact n

    {-# INLINE match #-}
    match :: [a] -> [a] -> Maybe [a]
    match xs = List.matchPrefix xs

    {-# INLINE singleton #-}
    singleton _ x = [x]
