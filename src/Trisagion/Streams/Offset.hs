{- |
Module: Trisagion.Streams.Offset

The @Offset@ type wrapping a 'Streamable' with an offset tracking current position.
-}

module Trisagion.Streams.Offset (
    -- * Streams.
    Offset,

    -- ** Constructors.
    initialize,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (null)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position.

The implementation initializes the offset to the length of the streamable and on each call to
'offset' takes the difference. This not only requires efficient implementation of 'monolength',
but can force the entire input stream into memory.
-}
data Offset s = Offset {-# UNPACK #-} !Word !s
    deriving stock (Eq, Show)

-- Instances.
instance MonoFunctor s => MonoFunctor (Offset s) where
    type ElementOf (Offset s) = ElementOf s

    {-# INLINE monomap #-}
    monomap :: (ElementOf s -> ElementOf s) -> Offset s -> Offset s
    monomap f (Offset n xs) = Offset n (monomap f xs)

instance MonoFoldable s => MonoFoldable (Offset s) where
    {-# INLINE monotoList #-}
    monotoList :: Offset s -> [ElementOf s]
    monotoList (Offset _ xs) = monotoList xs

instance Streamable s => Streamable (Offset s) where
    {-# INLINE uncons #-}
    uncons :: Offset s -> Maybe (ElementOf s, Offset s)
    uncons (Offset n xs) = fmap (Offset n) <$> uncons xs

    {-# INLINE null #-}
    null :: Offset s -> Bool
    null (Offset _ xs) = null xs

    {-# INLINE dropOne #-}
    dropOne :: Offset s -> Offset s
    dropOne (Offset n xs) = Offset n (dropOne xs)

    {-# INLINE toList #-}
    toList :: Offset s -> [ElementOf s]
    toList (Offset _ xs) = toList xs

instance Splittable s => Splittable (Offset s) where
    type PrefixOf (Offset s) = PrefixOf s

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Offset s -> (PrefixOf s, Offset s)
    splitPrefix n (Offset m xs) = Offset m <$> splitPrefix n xs

    {-# INLINE splitWith #-}
    splitWith :: (ElementOf s -> Bool) -> Offset s -> (PrefixOf s, Offset s)
    splitWith p (Offset m xs) = Offset m <$> splitWith p xs

    {-# INLINE single #-}
    single = single

instance (MonoFoldable s, Streamable s) => HasOffset (Offset s) where
    {-# INLINE offset #-}
    offset :: Offset s -> Word
    offset (Offset n xs) = n - monolength xs


{- | Construct an t'Offset' from a 'Streamable'. -}
{-# INLINE initialize #-}
initialize :: MonoFoldable s => s -> Offset s
initialize xs = Offset (monolength xs) xs
