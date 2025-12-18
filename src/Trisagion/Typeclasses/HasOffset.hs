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
class HasOffset (m :: Type -> Type) (s :: Type) where
    {-# MINIMAL offset #-}

    {- | Getter for the current offset of the stream. -}
    offset :: s -> m Word
