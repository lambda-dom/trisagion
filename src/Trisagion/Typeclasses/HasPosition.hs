{- |
Module: Trisagion.Typeclasses.HasPosition

The @HasPosition@ typeclass.
-}

module Trisagion.Typeclasses.HasPosition (
    -- * Typeclasses.
    HasPosition (..),
) where

-- Imports.
-- Base.
import Data.Kind (Type)


{- | The typeclass for input streams with a notion of current position. -}
class HasPosition s where
    {-# MINIMAL position #-}

    {- | The type of the stream's position. -}
    type PositionOf s :: Type

    {- | Getter for the current position of the stream. -}
    position :: s -> PositionOf s
