{- |
Module: Trisagion.Serializers.Combinators

Various serializer combinators.
-}

module Trisagion.Serializers.Combinators (
    -- * Serializers.
    constant,
    value,

    -- * Tuple combinators.
    pair,
    pairWith,

    -- * List combinators.
    many,
    repeatN,
) where

-- Imports.
-- Base.
import Data.Foldable (Foldable (..))
import Data.Functor.Contravariant (Contravariant (..))

-- Libraries.
import Data.Functor.Contravariant.Divisible (divided, divide)

-- Package.
import Trisagion.Serializer (Serializer, embed, run)


{- | The constant serializer. -}
{-# INLINE constant #-}
constant :: s -> Serializer s a
constant s = embed $ const s

{- | Embed a value in a serializer as a (constant) serializer. -}
{-# INLINE value #-}
value :: Serializer s a -> a -> Serializer s b
value s x = contramap (const x) s


{- | Pair two serializers for a serializer of pairs. -}
{-# INLINE pair #-}
pair :: Monoid s => Serializer s a -> Serializer s b -> Serializer s (a, b)
pair = divided

{- | Pair two serializers with a span @a -> (b, c)@. -}
{-# INLINE pairWith #-}
pairWith :: Monoid s => (a -> (b, c)) -> Serializer s b -> Serializer s c -> Serializer s a
pairWith = divide


{- | Serializer for foldables. -}
{-# INLINE many #-}
many :: (Foldable t, Monoid s) => Serializer s a -> Serializer s (t a)
many s = embed $ foldMap' (run s)

{- | Serializer that repeats @s@ @n@ times. -}
{-# INLINE repeatN #-}
repeatN :: Monoid s => Int -> Serializer s a -> Serializer s a
repeatN n s = foldMap' id $ replicate n s
