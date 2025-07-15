{- |
Module: Trisagion.Serializers.Combinators

Various serializer combinators.
-}

module Trisagion.Serializers.Combinators (
    -- * List combinators.
    listOf,
) where

-- Imports.
-- Package.
import Trisagion.Serializer (Serializer, embed, serialize)


{- | Serializer for lists. -}
{-# INLINEABLE listOf #-}
listOf :: Monoid m => Serializer m a -> Serializer m [a]
listOf h = embed $ foldMap (serialize h) 
