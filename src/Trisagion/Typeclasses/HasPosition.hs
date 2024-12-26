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

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | The typeclass for streamables with a notion of position. -}
class Streamable s => HasPosition s where
    {-# MINIMAL position #-}

    {- | The type of the streamable's position. -}
    type PositionOf s :: Type

    {- | Return the current position of the stream. -}
    position :: s -> PositionOf s
