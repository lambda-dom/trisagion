{-# LANGUAGE LambdaCase #-}

{- |
Module: Trisagion.Serializer

The @Serializer@ contravariant functor.
-}

module Trisagion.Serializer (
    -- * The serializer type.
    Serializer (..),

    -- ** Basic functions.
    run,

    -- * Serializers.
    either,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (either)

-- Base.
import Data.Functor.Contravariant (Contravariant (..))

-- Package.
import Trisagion.Parser ((:+:))


{- | The serializer type. -}
newtype Serializer m a = Serializer (a -> m)


-- Instances.
instance Contravariant (Serializer m) where
    contramap :: (a -> b) -> Serializer m b -> Serializer m a
    contramap f (Serializer m) = Serializer (m . f)

instance Semigroup m => Semigroup (Serializer m a) where
    (<>) :: Serializer m a -> Serializer m a -> Serializer m a
    (<>) (Serializer m) (Serializer n) = Serializer (\ x -> m x <> n x)

instance Monoid m => Monoid (Serializer m a) where
    mempty :: Serializer m a
    mempty = Serializer (const mempty)


{- | Run a serializer on the input and return the result. -}
run :: Serializer m a -> a -> m
run (Serializer m) = m


{- | Representability isomorphism. -}
either :: Serializer m a -> Serializer m b -> Serializer m (a :+: b)
either (Serializer m) (Serializer n)
    = Serializer $
        \case 
            Left x  -> m x
            Right y -> n y
