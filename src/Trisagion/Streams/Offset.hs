{- |
Module: Trisagion.Streams.Offset

The @Offset@ type wrapping a 'Streamable' with an offset tracking current position.
-}

module Trisagion.Streams.Offset (
    -- * Types.
    Offset,

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


{- | Wrapper around a 'Streamable' adding an offset to track current position.

The implementation initializes the offset to the length of the streamable and on each call to @getPosition@
takes the difference. This not only requires efficient implementation of 'olength', but can force all
the streamable into memory.
-}
data Offset s = Offset !Word !s
    deriving stock (Eq, Show)


-- Associated type.
type instance Element (Offset s) = Element s

-- Instances.
instance MonoFunctor s => MonoFunctor (Offset s) where
    {-# INLINE omap #-}
    omap :: (Element (Offset s) -> Element (Offset s)) -> Offset s -> Offset s
    omap f (Offset l xs) = Offset l (omap f xs)

instance (MonoFoldable s) => MonoFoldable (Offset s) where
    {-# INLINE ofoldMap #-}
    ofoldMap :: Monoid m => (Element (Offset s) -> m) -> Offset s -> m
    ofoldMap f (Offset _ xs) = ofoldMap f xs

    {-# INLINE ofoldr #-}
    ofoldr :: (Element (Offset s) -> a -> a) -> a -> Offset s -> a
    ofoldr f x (Offset _ xs) = ofoldr f x xs

    {-# INLINE ofoldl' #-}
    ofoldl' :: (a -> Element (Offset s) -> a) -> a -> Offset s -> a
    ofoldl' f x (Offset _ xs) = ofoldl' f x xs

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex
        :: (Element (Offset s) -> Element (Offset s) -> Element (Offset s))
        -> Offset s
        -> Element (Offset s)
    ofoldr1Ex f (Offset _ xs) = ofoldr1Ex f xs

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex'
        :: (Element (Offset s) -> Element (Offset s) -> Element (Offset s))
        -> Offset s
        -> Element (Offset s)
    ofoldl1Ex' f (Offset _ xs) = ofoldl1Ex' f xs

instance Streamable s => Streamable (Offset s) where
    {-# INLINE getOne #-}
    getOne :: Offset s -> Maybe (Element (Offset s), Offset s)
    getOne (Offset l xs) = second (Offset l) <$> getOne xs

instance Splittable s => Splittable (Offset s) where
    type PrefixOf (Offset s) = PrefixOf s

    {-# INLINE getAt #-}
    getAt :: Word -> Offset s -> (PrefixOf (Offset s), Offset s)
    getAt n (Offset l xs) = second (Offset l) $ getAt n xs

    {-# INLINE getWith #-}
    getWith :: (Element (Offset s) -> Bool) -> Offset s -> (PrefixOf (Offset s), Offset s)
    getWith p (Offset l xs) = second (Offset l) $ getWith p xs

instance Streamable s => HasPosition (Offset s) where
    type PositionOf (Offset s) = Word

    {-# INLINE getPosition #-}
    getPosition :: Offset s -> Word
    getPosition (Offset n xs) = n - fromIntegral (olength xs)


{- | Construct an t'Offset' from a 'Streamable'. -}
{-# INLINE initialize #-}
initialize :: MonoFoldable s => s -> Offset s
initialize xs = Offset (fromIntegral $ olength xs) xs
