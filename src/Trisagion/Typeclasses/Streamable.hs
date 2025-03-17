{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),
) where

-- Imports.
-- Base.
import Data.Kind (Type)



{- | The @Streamable@ typeclass of monomorphic, streamable functors. -}
class Streamable s where
    {-# MINIMAL getOne #-}

    type ElementOf s :: Type

    {- | Get, or uncons, the first element from the streamable. -}
    getOne :: s -> Maybe (ElementOf s, s)
