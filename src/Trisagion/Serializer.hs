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
    contramap f s = embed $ run s . f

instance Monoid m => Divisible (Serializer m) where
    conquer :: Serializer m a
    conquer = mempty

    divide :: forall a b c . (c -> a :*: b) -> Serializer m a -> Serializer m b -> Serializer m c
    divide f s t = embed $ g . f
        where
            g :: a :*: b -> m
            g = uncurry (<>) . bimap (run s) (run t)

instance Monoid m => Decidable (Serializer m) where
    lose :: (a -> Void) -> Serializer m a
    lose f = embed $ absurd . f

    choose :: (a -> b :+: c) -> Serializer m b -> Serializer m c -> Serializer m a
    choose f s t = embed $ choice (run s) (run t) . f
        where
            -- | Representability isomorphism.
            choice :: (a -> m) -> (b -> m) -> (a :+: b) -> m
            choice p q
                = \case 
                    Left x  -> p x
                    Right y -> q y

instance Semigroup m => Semigroup (Serializer m a) where
    (<>) :: Serializer m a -> Serializer m a -> Serializer m a
    (<>) s t = embed $ \ x -> run s x <> run t x

instance Monoid m => Monoid (Serializer m a) where
    mempty :: Serializer m a
    mempty = embed $ const mempty


{- | Run a serializer on the input and return the result. -}
run :: Serializer m a -> a -> m
run (Serializer m) = m

{- | Embed a serializing function in a 'Serializer'.

The inverse to 'run'.
-}
embed :: (a -> m) -> Serializer m a
embed = Serializer


{- | Monoid action for serializers. -}
(|*>) :: Monoid m => m -> Serializer m a -> Serializer m a
(|*>) m s = embed $ \ x -> m <> run s x
infixr 5 |*>
