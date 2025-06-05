{- |
Module: Trisagion.Serializer

The @Serializer@ contravariant functor.
-}

module Trisagion.Serializer (
    -- * The serializer type.
    Serializer,

    -- ** Basic functions.
    run,
    embed,

    -- * Operators.
    (|*>),
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))


{- | The serializer type. -}
newtype Serializer m a = Serializer (a -> m)


-- Instances.
instance Contravariant (Serializer m) where
    {-# INLINE contramap #-}
    contramap :: (a -> b) -> Serializer m b -> Serializer m a
    contramap f s = embed $ run s . f


{- | Run a serializer on the input and return the result. -}
{-# INLINE run #-}
run :: Serializer m a -> a -> m
run (Serializer m) = m

{- | Embed a serializing function in a 'Serializer'.

The inverse to 'run'.
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
(|*>) m s = embed $ \ x -> m <> run s x
infixr 5 |*>
