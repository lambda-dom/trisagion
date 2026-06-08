{- |
Module: Trisagion.Serializers.Combinators

Various serializer combinators.
-}

module Trisagion.Serializers.Combinators (
    -- * List combinators.
    many,
) where

-- Imports.
-- Package.
import Trisagion.Serializer (Serializer, embed, run)


{- | Serializer for lists. -}
{-# INLINE many #-}
many :: Monoid s => Serializer s a -> Serializer s [a]
many s = embed $ foldMap (run s)
