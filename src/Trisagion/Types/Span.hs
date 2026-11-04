{- |
Module: Trisagion.Parsers.Span

Parsers with span information.
-}

module Trisagion.Types.Span (
    -- * Types.
    Span (..),

    -- ** Functions.
    width,
    toList,

    -- ** Predicates.
    isEmpty,
    isElem,
) where

-- Imports.
-- Base.
import Data.Kind (Type)


{- | The @Span@ type.

A /span/ is a closed-open interval of the form @[m, n[@ with @m, n :: 'Int'@.
-}
type Span :: Type
data Span = Span {
    lower :: {-# UNPACK #-} !Int,
    upper :: {-# UNPACK #-} !Int
} deriving stock (Eq, Show)


{- | Return the width of the span. -}
width :: Span -> Int
width (Span n m) = m - n

{- | Return the list of elements in the span. -}
toList :: Span -> [Int]
toList (Span m n) = [m .. pred n]


{- | Return 'True' if the spane is empty. -}
isEmpty :: Span -> Bool
isEmpty (Span m n) = m >= n

{- | Return 'True' if elem is in the span. -}
isElem :: Int -> Span -> Bool
isElem x (Span m n) = m <= x && x < n
