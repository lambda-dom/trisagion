{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Trisagion.Streams.Counter

The @Counter@ type wrapping a 'Streamable' with an offset tracking current position.
-}

module Trisagion.Streams.Counter (
    -- * Streams.
    Counter,

    -- ** Constructors.
    initialize,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (null, splitAt)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position.

The implementation initializes the counter to @0@ and then updates it on every streamable operation
by computing the length of the prefix.
-}
data Counter s = Counter {-# UNPACK #-} !Word !s
    deriving stock (Eq, Show)


-- Instances.
instance MonoFunctor s => MonoFunctor (Counter s) where
    type ElementOf (Counter s) = ElementOf s

    {-# INLINE monomap #-}
    monomap :: (ElementOf s -> ElementOf s) -> Counter s -> Counter s
    monomap f (Counter n xs) = Counter n (monomap f xs)

instance Streamable s => Streamable (Counter s) where
    {-# INLINE uncons #-}
    uncons :: Counter s -> Maybe (ElementOf s, Counter s)
    uncons (Counter n xs) =
        case uncons xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (y, Counter (succ n) ys)

    {-# INLINE null #-}
    null :: Counter s -> Bool
    null (Counter _ xs) = null xs

    {-# INLINE toList #-}
    toList :: Counter s -> [ElementOf s]
    toList (Counter _ xs) = toList xs

instance Streamable s => HasOffset (Counter s) where
    {-# INLINE offset #-}
    offset :: Counter s -> Word
    offset (Counter n _) = n

{- | 'Splittable' instance.

The instance requires computing the length of the prefix, which is @O(n)@ for some types like
@Text@. This in its turn, requires a @'MonoFoldable' ('PrefixOf' s)@ constraint and the
@UndecidableInstances@ extension to shut up GHC.
-}
instance (Splittable s, MonoFoldable (PrefixOf s)) => Splittable (Counter s) where
    type PrefixOf (Counter s) = PrefixOf s
 
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Counter s -> (PrefixOf s, Counter s)
    splitPrefix n (Counter off xs) =
        let (prefix, rest) = splitPrefix n xs in
            (prefix, Counter (off + monolength prefix) rest)

    {-# INLINE splitWith #-}
    splitWith :: (ElementOf s -> Bool) -> Counter s -> (PrefixOf s, Counter s)
    splitWith p (Counter off xs) =
        let (prefix, rest) = splitWith p xs in
            (prefix, Counter (off + monolength prefix) rest)

    {-# INLINE single #-}
    single :: ElementOf (Counter s) -> PrefixOf (Counter s)
    single = single @s

    {-# INLINE splitRemainder #-}
    splitRemainder :: Counter s -> (PrefixOf s, Counter s)
    splitRemainder (Counter off xs) =
        let (prefix, rest) = splitRemainder xs in
            (prefix, Counter (off + monolength prefix) rest)


{- | Construct a t'Counter' from a 'Streamable'. -}
{-# INLINE initialize #-}
initialize :: s -> Counter s
initialize = Counter 0
