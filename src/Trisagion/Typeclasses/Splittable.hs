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
import qualified Trisagion.Utils.List as List (splitAtExact, matchPrefix)
import Trisagion.Typeclasses.Streamable (Streamable)


{- | The @Splittable@ typeclass. -}
class Streamable a s => Splittable a b s | s -> b where
    {-# MINIMAL splitPrefix, splitWith, singleton, splitPrefixExact, matchPrefix #-}

    {- | Split a fixed size prefix from the stream. -}
    splitPrefix :: Word -> s -> (b, s)

    {- | Split the longest prefix from the stream whose elements satisfy a predicate. -}
    splitWith :: (a -> Bool) -> s -> (b, s)

    {- | Pure function to convert a stream element to a prefix. -}
    singleton :: forall t -> s ~ t => a -> b

    {- | Split a prefix of exact size. -}
    splitPrefixExact :: Word -> s -> Maybe (b, s)

    {- | Parse and drop a matching prefix. -}
    matchPrefix :: b -> s -> Maybe s


-- Instances.
instance Eq a => Splittable a [a] [a] where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> [a] -> ([a], [a])
    splitPrefix n = splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith p = span p

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> [a] -> Maybe ([a], [a])
    splitPrefixExact n = List.splitAtExact n

    {-# INLINE matchPrefix #-}
    matchPrefix :: [a] -> [a] -> Maybe [a]
    matchPrefix xs = List.matchPrefix xs

    {-# INLINE singleton #-}
    singleton _ x = [x]
