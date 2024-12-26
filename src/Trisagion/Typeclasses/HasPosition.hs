{- |
Module: Trisagion.Typeclasses.HasPosition

The @HasPosition@ typeclass.
-}

module Trisagion.Typeclasses.HasPosition (
    -- * Typeclasses.
    HasPosition (..),

    -- * Types.
    Position,

    -- ** Constructors.
    initialize,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)

-- Libraries.
import Data.MonoTraversable (Element, MonoFunctor (..), MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))


{- | The typeclass for streamables with a notion of position. -}
class Streamable s => HasPosition s where
    {-# MINIMAL getPosition #-}

    {- | The type of the streamable's position. -}
    type PositionOf s :: Type

    {- | Return the current position of the stream. -}
    getPosition :: s -> PositionOf s


{- | newtype wrapper around a streamable with itself as the current position. -}
newtype Position s = Position s
    deriving stock (Eq, Show)


{- | Construct a 'Position' from a 'Streamable'. -}
initialize :: s -> Position s
initialize = Position


-- Associated type.
type instance Element (Position s) = Element s

-- Instances.
instance MonoFunctor s => MonoFunctor (Position s) where
    omap :: (Element (Position s) -> Element (Position s)) -> Position s -> Position s
    omap f (Position xs) = Position (omap f xs)

instance (MonoFoldable s) => MonoFoldable (Position s) where
    ofoldMap :: Monoid m => (Element (Position s) -> m) -> Position s -> m
    ofoldMap f (Position xs) = ofoldMap f xs

    ofoldr :: (Element (Position s) -> a -> a) -> a -> Position s -> a
    ofoldr f x (Position xs) = ofoldr f x xs

    ofoldl' :: (a -> Element (Position s) -> a) -> a -> Position s -> a
    ofoldl' f x (Position xs) = ofoldl' f x xs

    ofoldr1Ex
        :: (Element (Position s) -> Element (Position s) -> Element (Position s))
        -> Position s
        -> Element (Position s)
    ofoldr1Ex f (Position xs) = ofoldr1Ex f xs

    ofoldl1Ex'
        :: (Element (Position s) -> Element (Position s) -> Element (Position s))
        -> Position s
        -> Element (Position s)
    ofoldl1Ex' f (Position xs) = ofoldl1Ex' f xs

instance Streamable s => Streamable (Position s) where
    getOne :: Position s -> Maybe (Element (Position s), Position s)
    getOne (Position xs) = second Position <$> getOne xs

instance Streamable s => HasPosition (Position s) where
    type PositionOf (Position s) = s

    getPosition :: Position s -> s
    getPosition (Position s) = s
