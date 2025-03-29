{-# LANGUAGE UndecidableInstances #-}

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
-- Prelude hiding.
import Prelude hiding (splitAt)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoFoldable (MonoFoldable(..))

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position.

The implementation initializes the counter to @0@ and then updates it on every 'Splittable' operation
by computing the length of the prefix.
-}
data Counter s = Counter !Word !s
    deriving stock (Eq, Show)


-- Instances.
instance MonoFunctor s => MonoFunctor (Counter s) where
    type ElementOf (Counter s) = ElementOf s

    monomap :: (ElementOf s -> ElementOf s) -> Counter s -> Counter s
    monomap f (Counter n xs) = Counter n (monomap f xs)

instance HasPosition (Counter s) where
    type PositionOf (Counter s) = Word

    getPosition :: Counter s -> Word
    getPosition (Counter n _) = n

instance Streamable s => Streamable (Counter s) where
    splitOne :: Counter s -> Maybe (ElementOf (Counter s), Counter s)
    splitOne (Counter n xs) =
        case splitOne xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (y, Counter (succ n) ys)

{- | 'Splittable' instance.

The instance requires computing the length of the prefix, which is @O(n)@ for some types like
@Text@. This in its turn, requires a @'MonoFoldable' ('PrefixOf' s)@ constraint and the
@UndecidableInstances@ extension to keep GHC happy.
-}
instance (Splittable s, MonoFoldable (PrefixOf s)) => Splittable (Counter s) where
    type PrefixOf (Counter s) = PrefixOf s
 
    splitAt :: Word -> Counter s -> (PrefixOf (Counter s), Counter s)
    splitAt n (Counter offset xs) =
        let
            (prefix, suffix) = splitAt n xs
        in
            (prefix, Counter (offset + monolength prefix) suffix)

    splitWith :: (ElementOf (Counter s) -> Bool) -> Counter s -> (PrefixOf (Counter s), Counter s)
    splitWith p (Counter offset xs) =
        let
            (prefix, suffix) = splitWith p xs
        in
            (prefix, Counter (offset + monolength prefix) suffix)


{- | Construct a t'Counter' from a 'Streamable'. -}
initialize :: s -> Counter s
initialize = Counter 0
