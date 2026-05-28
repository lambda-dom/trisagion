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
-- Base.
import Data.Kind (Type)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import qualified Trisagion.Typeclasses.Streamable as Streamable (null)
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position.

The implementation initializes the counter to @0@ and then updates it on every streamable operation
by computing the length of the prefix.
-}
type Counter:: Type -> Type
data Counter s = Counter {-# UNPACK #-} !Word !s
    deriving stock (Eq, Show)


-- Instances.
instance MonoFunctor a s => MonoFunctor a (Counter s) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Counter s -> Counter s
    monomap f (Counter n xs) = Counter n (monomap f xs)

instance Streamable a s => Streamable a (Counter s) where
    {-# INLINE uncons #-}
    uncons :: Counter s -> Maybe (a, Counter s)
    uncons (Counter n xs) =
        case uncons xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (y, Counter (succ n) ys)

    {-# INLINE null #-}
    null :: Counter s -> Bool
    null (Counter _ xs) = Streamable.null xs

    {-# INLINE toList #-}
    toList :: Counter s -> [a]
    toList (Counter _ xs) = toList xs

instance Streamable a s => HasOffset (Counter s) where
    {-# INLINE offset #-}
    offset :: Counter s -> Word
    offset (Counter n _) = n

{- | 'Splittable' instance.

The instance requires computing the length of the prefix, which is @O(n)@ for some types like
@Text@. This in its turn, requires a @'MonoFoldable' b@ constraint and the
@UndecidableInstances@ extension to shut up GHC.
-}
instance (Splittable a b s, MonoFoldable a b) => Splittable a b (Counter s) where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Counter s -> (b, Counter s)
    splitPrefix n (Counter off xs) =
        let (prefix, rest) = splitPrefix n xs in
            (prefix, Counter (off + monolength prefix) rest)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Counter s -> (b, Counter s)
    splitWith p (Counter off xs) =
        let (prefix, rest) = splitWith p xs in
            (prefix, Counter (off + monolength prefix) rest)

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> Counter s -> Maybe (b, Counter s)
    splitPrefixExact n (Counter off xs) =
        case splitPrefixExact n xs of
            Nothing        -> Nothing
            Just (ys, zs)  -> Just (ys, Counter (off + monolength ys) zs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: b -> Counter s -> Maybe (Counter s)
    matchPrefix xs (Counter off ys) =
        case matchPrefix xs ys of
            Nothing -> Nothing
            Just zs -> Just (Counter (off + monolength xs) zs)

    {-# INLINE singleton #-}
    singleton (type (Counter t)) = singleton t

    -- {-# INLINE splitRemainder #-}
    -- splitRemainder :: Counter s -> (b, Counter s)
    -- splitRemainder (Counter off xs) =
    --     let (prefix, rest) = splitRemainder xs in
    --         (prefix, Counter (off + monolength prefix) rest)


{- | Construct a t'Counter' from a 'Streamable'. -}
{-# INLINE initialize #-}
initialize :: s -> Counter s
initialize = Counter 0
