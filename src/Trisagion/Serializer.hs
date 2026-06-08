{- |
Module: Trisagion.Serializer

The @Serializer@ contravariant functor.
-}

module Trisagion.Serializer (
    -- * Types.
    Serializer,

    -- ** Basic functions.
    embed,
    run,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))

-- Libraries.
import Data.Functor.Contravariant.Divisible (Divisible (..))


{- | The @Serializer@ type. -}
newtype Serializer s a = Serializer (a -> s)


-- Instances.
instance Semigroup s => Semigroup (Serializer s a) where
    {-# INLINE (<>) #-}
    (<>) :: Serializer s a -> Serializer s a -> Serializer s a
    (<>) s t = embed $ \ x -> run s x <> run t x

instance Monoid s => Monoid (Serializer s a) where
    {-# INLINE mempty #-}
    mempty :: Serializer s a
    mempty = embed $ const mempty

instance Contravariant (Serializer s) where
    {-# INLINE contramap #-}
    contramap :: (a -> b) -> Serializer s b -> Serializer s a
    contramap f s = embed $ run s . f

instance Monoid s => Divisible (Serializer s) where
    {-# INLINE conquer #-}
    conquer :: Serializer s a
    conquer = mempty

    {-# INLINE divide #-}
    divide :: forall a b c . (c -> (a, b)) -> Serializer s a -> Serializer s b -> Serializer s c
    divide f s t = embed $ g . f
        where
            g :: (a, b) -> s
            g = uncurry (<>) . bimap (run s) (run t)


{- | Embed a serializing function in a t'Serializer'. -}
{-# INLINE embed #-}
embed :: (a -> s) -> Serializer s a
embed = Serializer

{- | Run a serializer on the input and return the result.

The inverse of 'embed'.
-}
{-# INLINE run #-}
run :: Serializer s a -> a -> s
run (Serializer f) = f
