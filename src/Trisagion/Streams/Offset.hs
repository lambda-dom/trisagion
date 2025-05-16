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
-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position.

The implementation initializes the offset to the length of the streamable and on each call to
@offset@ takes the difference. This not only requires efficient implementation of 'monolength',
but can force the entire input stream into memory.
-}
data Offset s = Offset !Word !s
    deriving stock (Eq, Show)

-- Instances.
instance MonoFunctor s => MonoFunctor (Offset s) where
    type ElementOf (Offset s) = ElementOf s

    {-# INLINE monomap #-}
    monomap :: (ElementOf s -> ElementOf s) -> Offset s -> Offset s
    monomap f (Offset l xs) = Offset l (monomap f xs)

instance Streamable s => Streamable (Offset s) where
    {-# INLINE uncons #-}
    uncons :: Offset s -> Maybe (ElementOf s, Offset s)
    uncons (Offset l xs) = fmap (Offset l) <$> uncons xs

instance MonoFoldable s => HasOffset (Offset s) where
    {-# INLINE offset #-}
    offset :: Offset s -> Word
    offset (Offset n xs) = n - monolength xs


{- | Construct an t'Offset' from a 'Streamable'. -}
{-# INLINE initialize #-}
initialize :: MonoFoldable s => s -> Offset s
initialize xs = Offset (monolength xs) xs
