{- |
Module: Trisagion.Streams.Counter

The @Counter@ type wrapping a 'Streamable' with an offset tracking current position.
-}

module Trisagion.Streams.Counter (
    -- * Types.
    Counter,

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
import Trisagion.Typeclasses.HasPosition (HasPosition (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position. -}
data Counter s = Counter !Word !s
    deriving stock (Eq, Show)


-- Associated type.
type instance Element (Counter s) = Element s

-- Instances.
instance MonoFunctor s => MonoFunctor (Counter s) where
    {-# INLINE omap #-}
    omap :: (Element (Counter s) -> Element (Counter s)) -> Counter s -> Counter s
    omap f (Counter off xs) = Counter off (omap f xs)

instance (MonoFoldable s) => MonoFoldable (Counter s) where
    {-# INLINE ofoldMap #-}
    ofoldMap :: Monoid m => (Element (Counter s) -> m) -> Counter s -> m
    ofoldMap f (Counter _ xs) = ofoldMap f xs

    {-# INLINE ofoldr #-}
    ofoldr :: (Element (Counter s) -> a -> a) -> a -> Counter s -> a
    ofoldr f x (Counter _ xs) = ofoldr f x xs

    {-# INLINE ofoldl' #-}
    ofoldl' :: (a -> Element (Counter s) -> a) -> a -> Counter s -> a
    ofoldl' f x (Counter _ xs) = ofoldl' f x xs

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex
        :: (Element (Counter s) -> Element (Counter s) -> Element (Counter s))
        -> Counter s
        -> Element (Counter s)
    ofoldr1Ex f (Counter _ xs) = ofoldr1Ex f xs

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex'
        :: (Element (Counter s) -> Element (Counter s) -> Element (Counter s))
        -> Counter s
        -> Element (Counter s)
    ofoldl1Ex' f (Counter _ xs) = ofoldl1Ex' f xs

instance Streamable s => Streamable (Counter s) where
    {-# INLINE getOne #-}
    getOne :: Counter s -> Maybe (Element (Counter s), Counter s)
    getOne (Counter offset xs) = second (Counter (succ offset)) <$> getOne xs

instance Streamable s => HasPosition (Counter s) where
    type PositionOf (Counter s) = Word

    {-# INLINE getPosition #-}
    getPosition :: Counter s -> Word
    getPosition (Counter n _) = n


{- | Construct a t'Counter' from a 'Streamable'. -}
{-# INLINE initialize #-}
initialize :: s -> Counter s
initialize = Counter 0
