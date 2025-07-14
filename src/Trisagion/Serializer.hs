{- |
Module: Trisagion.Serializer

The @Serializer@ contravariant functor.
-}

module Trisagion.Serializer (
    -- * The serializer type.
    Serializer,

    -- ** Basic functions.
    serialize,
    embed,

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


{- | The @Serializer@ type. -}
newtype Serializer m a = Serializer (a -> m)


-- Instances.
instance Semigroup m => Semigroup (Serializer m a) where
    {-# INLINE (<>) #-}
    (<>) :: Serializer m a -> Serializer m a -> Serializer m a
    (<>) s t = embed $ \ x -> serialize s x <> serialize t x

instance Monoid m => Monoid (Serializer m a) where
    {-# INLINE mempty #-}
    mempty :: Serializer m a
    mempty = embed $ const mempty

instance Contravariant (Serializer m) where
    {-# INLINE contramap #-}
    contramap :: (a -> b) -> Serializer m b -> Serializer m a
    contramap f s = embed $ serialize s . f

instance Monoid m => Divisible (Serializer m) where
    {-# INLINE conquer #-}
    conquer :: Serializer m a
    conquer = mempty

    {-# INLINE divide #-}
    divide :: forall a b c . (c -> (a, b)) -> Serializer m a -> Serializer m b -> Serializer m c
    divide f s t = embed $ g . f
        where
            g :: (a, b) -> m
            g = uncurry (<>) . bimap (serialize s) (serialize t)

instance Monoid m => Decidable (Serializer m) where
    {-# INLINE lose #-}
    lose :: (a -> Void) -> Serializer m a
    lose f = embed $ absurd . f

    {-# INLINE choose #-}
    choose :: (a -> Either b c) -> Serializer m b -> Serializer m c -> Serializer m a
    choose f s t = embed $ choice (serialize s) (serialize t) . f
        where
            -- Representability isomorphism.
            choice :: (a -> m) -> (b -> m) -> Either a b -> m
            choice p q r = case r of
                Left  x -> p x
                Right y -> q y


{- | Run a serializer on the input and return the result. -}
{-# INLINE serialize #-}
serialize :: Serializer m a -> a -> m
serialize (Serializer m) = m

{- | Embed a serializing function in a t'Serializer'.

The inverse to 'serialize'.
-}
{-# INLINE embed #-}
embed :: (a -> m) -> Serializer m a
embed = Serializer


{- | Monoid action for serializers.

The binary operator @(|*>)@ satisfies the (left) /monoid action/ laws:

prop> mempty |*> p == p
prop> m |*> (n |*> p) == (m <> n) |*> p
-}
{-# INLINE (|*>) #-}
(|*>) :: Monoid m => m -> Serializer m a -> Serializer m a
(|*>) m s = embed $ (m <>) . serialize s
infixr 5 |*>
