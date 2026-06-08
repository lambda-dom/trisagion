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

    -- * Operators.
    (|*>),
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Void (Void, absurd)

-- Libraries.
import Data.Functor.Contravariant.Divisible (Divisible (..), Decidable (..))

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))


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
    divide :: (c -> (a, b)) -> Serializer s a -> Serializer s b -> Serializer s c
    divide f s t = embed $ uncurry (<>) . bimap (run s) (run t) . f

instance Monoid s => Decidable (Serializer s) where
    {-# INLINE lose #-}
    lose :: (a -> Void) -> Serializer s a
    lose f = embed $ absurd . f

    {-# INLINE choose #-}
    choose :: (a -> b :+: c) -> Serializer s b -> Serializer s c -> Serializer s a
    choose f s t = embed $ choice (run s) (run t) . f
        where
            -- Representability isomorphism.
            choice :: (a -> s) -> (b -> s) -> a :+: b -> s
            choice p q r = case r of
                Left  x -> p x
                Right y -> q y


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


{- | Monoid action for serializers.

The binary operator @(|*>)@ satisfies the (left) /monoid action/ laws:

@
mempty |*> p == p
(m <> n) |*> p == m |*> (n |*> p)
@
-}
{-# INLINE (|*>) #-}
(|*>) :: Monoid s => s -> Serializer s a -> Serializer s a
(|*>) m s = embed $ (m <>) . run s
infixr 5 |*>
