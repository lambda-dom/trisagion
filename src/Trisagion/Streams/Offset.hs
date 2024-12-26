{- |
Module: Trisagion.Streams.Offset

The @Stream@ type wrapping a 'Streamable' with an offset tracking current position.
-}

module Trisagion.Streams.Offset (
    -- * Types.
    Stream,

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

note(s):

    * Requires efficient implementation of 'olength'; e. g. streamables like @Text@ are not appropriate.
-}
data Stream s = Stream !Word !s
    deriving stock (Eq, Show)


-- Associated type.
type instance Element (Stream s) = Element s

-- Instances.
instance MonoFunctor s => MonoFunctor (Stream s) where
    omap :: (Element (Stream s) -> Element (Stream s)) -> Stream s -> Stream s
    omap f (Stream l xs) = Stream l (omap f xs)

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
    getOne (Stream l xs) = second (Stream l) <$> getOne xs

instance Splittable s => Splittable (Stream s) where
    type PrefixOf (Stream s) = PrefixOf s

    getAt :: Word -> Stream s -> (PrefixOf (Stream s), Stream s)
    getAt n (Stream l xs) = second (Stream l) $ getAt n xs

    getWith :: (Element (Stream s) -> Bool) -> Stream s -> (PrefixOf (Stream s), Stream s)
    getWith p (Stream l xs) = second (Stream l) $ getWith p xs

instance Streamable s => HasPosition (Stream s) where
    type PositionOf (Stream s) = Word

    getPosition :: Stream s -> Word
    getPosition (Stream n xs) = n - fromIntegral (olength xs)


{- | Construct a t'Stream' from a 'Streamable'. -}
initialize :: MonoFoldable s => s -> Stream s
initialize xs = Stream (fromIntegral $ olength xs) xs
