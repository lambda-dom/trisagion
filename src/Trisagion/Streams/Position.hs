
{- |
Module: Trisagion.Streams.Position

The @Position@ type wrapping a 'Streamable' with itself as the current position.
-}

module Trisagion.Streams.Position (
    -- * Types.
    Position,

    -- ** Constructors.
    initialize,
) where


-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))

-- Libraries.
import Data.MonoTraversable (Element, MonoFunctor (..), MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Typeclasses.HasPosition (HasPosition (..))


{- | Newtype wrapper around a streamable with itself as the current position. -}
newtype Position s = Position s
    deriving stock (Eq, Show)


-- Associated type.
type instance Element (Position s) = Element s

-- Instances.
instance MonoFunctor s => MonoFunctor (Position s) where
    {-# INLINE omap #-}
    omap :: (Element (Position s) -> Element (Position s)) -> Position s -> Position s
    omap f (Position xs) = Position (omap f xs)

instance (MonoFoldable s) => MonoFoldable (Position s) where
    {-# INLINE ofoldMap #-}
    ofoldMap :: Monoid m => (Element (Position s) -> m) -> Position s -> m
    ofoldMap f (Position xs) = ofoldMap f xs

    {-# INLINE ofoldr #-}
    ofoldr :: (Element (Position s) -> a -> a) -> a -> Position s -> a
    ofoldr f x (Position xs) = ofoldr f x xs

    {-# INLINE ofoldl' #-}
    ofoldl' :: (a -> Element (Position s) -> a) -> a -> Position s -> a
    ofoldl' f x (Position xs) = ofoldl' f x xs

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex
        :: (Element (Position s) -> Element (Position s) -> Element (Position s))
        -> Position s
        -> Element (Position s)
    ofoldr1Ex f (Position xs) = ofoldr1Ex f xs

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex'
        :: (Element (Position s) -> Element (Position s) -> Element (Position s))
        -> Position s
        -> Element (Position s)
    ofoldl1Ex' f (Position xs) = ofoldl1Ex' f xs

instance Streamable s => Streamable (Position s) where
    {-# INLINE getOne #-}
    getOne :: Position s -> Maybe (Element (Position s), Position s)
    getOne (Position xs) = second Position <$> getOne xs

instance Splittable s => Splittable (Position s) where
    type PrefixOf (Position s) = PrefixOf s

    {-# INLINE getAt #-}
    getAt :: Word -> Position s -> (PrefixOf (Position s), Position s)
    getAt n (Position xs) = second Position $ getAt n xs

    {-# INLINE getWith #-}
    getWith :: (Element (Position s) -> Bool) -> Position s -> (PrefixOf (Position s), Position s)
    getWith p (Position xs) = second Position $ getWith p xs

instance Streamable s => HasPosition (Position s) where
    type PositionOf (Position s) = s

    {-# INLINE getPosition #-}
    getPosition :: Position s -> s
    getPosition (Position s) = s


{- | Construct a 'Position' from a 'Streamable'. -}
{-# INLINE initialize #-}
initialize :: s -> Position s
initialize = Position
