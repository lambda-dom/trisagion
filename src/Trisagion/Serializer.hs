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
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (either)

-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Void (Void, absurd)

-- Libraries.
import Data.Functor.Contravariant.Divisible (Divisible (..), Decidable (..))

-- Package.
import Trisagion.Parser ((:+:), (:*:))


{- | The serializer type. -}
newtype Serializer m a = Serializer (a -> m)


-- Instances.
instance Contravariant (Serializer m) where
    contramap :: (a -> b) -> Serializer m b -> Serializer m a
    contramap f (Serializer m) = Serializer (m . f)

instance Monoid m => Divisible (Serializer m) where
    conquer :: Serializer m a
    conquer = mempty

    divide :: forall a b c . (c -> a :*: b) -> Serializer m a -> Serializer m b -> Serializer m c
    divide f (Serializer m) (Serializer n) = Serializer (g . f)
        where
            g :: a :*: b -> m
            g = uncurry (<>) . bimap m n

instance Monoid m => Decidable (Serializer m) where
    lose :: (a -> Void) -> Serializer m a
    lose f = Serializer $ absurd . f

    choose :: (a -> b :+: c) -> Serializer m b -> Serializer m c -> Serializer m a
    choose f (Serializer m) (Serializer n) = Serializer $ eitherD m n . f
        where
            -- | Representability isomorphism.
            eitherD :: (a -> m) -> (b -> m) -> (a :+: b) -> m
            eitherD p q
                = \case 
                    Left x  -> p x
                    Right y -> q y


instance Semigroup m => Semigroup (Serializer m a) where
    (<>) :: Serializer m a -> Serializer m a -> Serializer m a
    (<>) (Serializer m) (Serializer n) = Serializer (\ x -> m x <> n x)

instance Monoid m => Monoid (Serializer m a) where
    mempty :: Serializer m a
    mempty = Serializer $ const mempty


{- | Run a serializer on the input and return the result. -}
run :: Serializer m a -> a -> m
run (Serializer m) = m
