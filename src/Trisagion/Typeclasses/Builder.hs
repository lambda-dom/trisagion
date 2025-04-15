{- |
Module: Trisagion.Typeclasses.Builder

The @Builder@ typeclass for builders of streamables.
-}

module Trisagion.Typeclasses.Builder (
    -- * Typeclasses.
    Builder (..),
) where

-- Imports.
-- Base.
import Data.Foldable (foldMap')
import Data.Kind (Type)

-- non-Hackage libraries.
import Data.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | Typeclass for builders of streamable sequences. -}
class (Monoid m, Streamable (BuilderOf m)) => Builder m where
    {-# MINIMAL unpack, one #-}

    {- | The type of sequence that the builder builds. -}
    type BuilderOf m :: Type

    {- | The monoid-isomorphism with what the builder builds. -}
    unpack :: m -> BuilderOf m

    {- | Build from one @'ElementOf' ('BuilderOf' m)@. -}
    one :: ElementOf (BuilderOf m) -> m

    {- | Build from a foldable of many @'ElementOf' ('BuilderOf' m)@. -}
    many :: Foldable t => t (ElementOf (BuilderOf m)) -> m
    many = foldMap' one
