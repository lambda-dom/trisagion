{- |
Module: Trisagion.Serializers.Combinators

Various serializer combinators.
-}

module Trisagion.Serializers.Combinators (
    -- * Serializers.
    constant,
    applied,

    -- * List combinators.
    many,
    repeatN,
) where

-- Imports.
-- Base.
import Data.Foldable (Foldable (..))

-- Package.
import Trisagion.Serializer (Serializer, embed, run)


{- | The constant serializer. -}
{-# INLINE constant #-}
constant :: s -> Serializer s a
constant s = embed $ const s

{- | The application of a serializer as a (constant) serializer. -}
{-# INLINE applied #-}
applied :: Serializer s a -> a -> Serializer s b
applied s x = constant $ run s x


{- | Serializer for foldables. -}
{-# INLINE many #-}
many :: (Foldable t, Monoid s) => Serializer s a -> Serializer s (t a)
many s = embed $ foldMap' (run s)

{- | Serializer that repeats @s@ @n@ times. -}
{-# INLINE repeatN #-}
repeatN :: Monoid s => Int -> Serializer s a -> Serializer s a
repeatN n s = foldMap' id $ replicate n s
