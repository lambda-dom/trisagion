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


{- | The @Span@ type.

A /span/ is a closed-open interval of the form @[m, n[@ with @m, n :: 'Int'@. The span is /empty/
if @m >= n@.
-}
data Span = Span {
    lower :: {-# UNPACK #-} !Int,
    upper :: {-# UNPACK #-} !Int
} deriving stock (Eq, Show)


{- | Return the width of the span. -}
width :: Span -> Int
width (Span m n) = max 0 (n - m)

{- | Return the list of elements in the span. -}
toList :: Span -> [Int]
toList (Span m n) = [m .. pred n]


{- | Return 'True' if the spane is empty. -}
isEmpty :: Span -> Bool
isEmpty (Span m n) = m >= n

{- | Return 'True' if elem is in the span. -}
isElem :: Int -> Span -> Bool
isElem x (Span m n) = m <= x && x < n
