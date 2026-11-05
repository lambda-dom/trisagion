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


{- | Serializer for lists. -}
{-# INLINE many #-}
many :: Monoid s => Serializer s a -> Serializer s [a]
many s = embed $ \xs -> foldl' (<>) mempty $ fmap (run s) xs

{- | Serializer repeating @s@ @n@ times. -}
{-# INLINE repeatN #-}
repeatN :: Monoid s => Int -> Serializer s a -> Serializer s a
repeatN n s = fold $ replicate n s
