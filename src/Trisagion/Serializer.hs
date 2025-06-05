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


{- | The serializer type. -}
newtype Serializer m a = Serializer (a -> m)


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


{- | Monoid action for serializers. -}
(|*>) :: Monoid m => m -> Serializer m a -> Serializer m a
(|*>) m s = embed $ \ x -> m <> run s x
infixr 5 |*>
