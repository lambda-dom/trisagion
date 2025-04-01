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
-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Typeclasses.Streamable (Streamable (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position.

The implementation initializes the counter to @0@ and then updates it on every operation
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

    position :: Counter s -> Word
    position (Counter n _) = n

instance Streamable s => Streamable (Counter s) where
    uncons :: Counter s -> Maybe (ElementOf (Counter s), Counter s)
    uncons (Counter n xs) =
        case uncons xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (y, Counter (succ n) ys)


{- | Construct a t'Counter' from a 'Streamable'. -}
initialize :: s -> Counter s
initialize = Counter 0
