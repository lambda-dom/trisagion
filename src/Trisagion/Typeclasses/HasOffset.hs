{- |
Module: Trisagion.Typeclasses.HasOffset

The @HasOffset@ typeclass.
-}

module Trisagion.Typeclasses.HasOffset (
    -- * Typeclasses.
    HasOffset (..),
) where

-- Imports.
-- Base.
import Data.Kind (Type)


{- | The typeclass for input streams with a notion of current offset. -}
class HasOffset (s :: Type) where
    {-# MINIMAL offset #-}

    {- | Return the current stream offset. -}
    offset :: s -> Int
