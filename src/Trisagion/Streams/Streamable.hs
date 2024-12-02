{- |
Module: Trisagion.Streams.Streamable

The @Stream@ type wrapping a 'Streamable' with an offset tracking current position.
-}

module Trisagion.Streams.Streamable (
    -- * Types.
    Stream,

    -- ** Constructors.
    initialize,

    -- ** Getters.
    offset,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))

-- Libraries.
import Data.MonoTraversable (Element, MonoFunctor (..), MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position. -}
data Stream s = Stream !Word !s
    deriving stock (Eq, Show)


-- Associated type.
type instance Element (Stream s) = Element s

-- Instances.
instance MonoFunctor s => MonoFunctor (Stream s) where
    omap :: (Element (Stream s) -> Element (Stream s)) -> Stream s -> Stream s
    omap f (Stream off xs) = Stream off (omap f xs)

instance (MonoFoldable s) => MonoFoldable (Stream s) where
    ofoldMap :: Monoid m => (Element (Stream s) -> m) -> Stream s -> m
    ofoldMap f (Stream _ xs) = ofoldMap f xs

    ofoldr :: (Element (Stream s) -> a -> a) -> a -> Stream s -> a
    ofoldr f x (Stream _ xs) = ofoldr f x xs

    ofoldl' :: (a -> Element (Stream s) -> a) -> a -> Stream s -> a
    ofoldl' f x (Stream _ xs) = ofoldl' f x xs

    ofoldr1Ex
        :: (Element (Stream s) -> Element (Stream s) -> Element (Stream s))
        -> Stream s
        -> Element (Stream s)
    ofoldr1Ex f (Stream _ xs) = ofoldr1Ex f xs

    ofoldl1Ex'
        :: (Element (Stream s) -> Element (Stream s) -> Element (Stream s))
        -> Stream s
        -> Element (Stream s)
    ofoldl1Ex' f (Stream _ xs) = ofoldl1Ex' f xs

instance Streamable s => Streamable (Stream s) where
    getOne :: Stream s -> Maybe (Element (Stream s), Stream s)
    getOne (Stream off xs) = second (Stream (succ off)) <$> getOne xs


{- | Construct a t'Stream' from a 'Streamable'. -}
initialize :: s -> Stream s
initialize = Stream 0

{- | Return the current offset of the t'Stream'. -}
offset :: Stream s -> Word
offset (Stream off _) = off
