{- |
Module: Trisagion.Parsers.Span

Parsers with span information.
-}

module Trisagion.Types.Span (
    -- * Types.
    Span (..),

    -- ** Functions.
    width,
) where

-- Imports.
-- Base.
import Data.Kind (Type)


{- | The @Span@ type. -}
type Span :: Type
data Span = Span {
    lower :: {-# UNPACK #-} !Int,
    upper :: {-# UNPACK #-} !Int
} deriving stock (Eq, Show)


{- | Return the width of the span. -}
width :: Span -> Int
width (Span n m) = m - n
