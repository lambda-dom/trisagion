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
import Data.Kind (Type)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | Typeclass for builders of streamable sequences. -}
class (Monoid m, Streamable (BuilderOf m)) => Builder m where
    {-# MINIMAL unpack, pack, one #-}

    {- | The type of sequence that the builder builds. -}
    type BuilderOf m :: Type

    {- | The monoid-isomorphism with what the builder builds. -}
    unpack :: m -> BuilderOf m

    {- | The inverse of 'unpack'. -}
    pack :: BuilderOf m -> m

    {- | Build from one @'ElementOf' ('BuilderOf' m)@. -}
    one :: ElementOf (BuilderOf m) -> m

    {- | Build from a foldable of many @'ElementOf' ('BuilderOf' m)@.

    Default implementation is:

    @many = foldMap one@
    -}
    many :: Foldable t => t (ElementOf (BuilderOf m)) -> m
    many = foldMap one

