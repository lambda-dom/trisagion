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
import Data.Void (Void)

-- Package.
import Trisagion.ParserT (ParserT)


{- | The typeclass for input streams with a notion of current offset. -}
class HasOffset (m :: Type -> Type) (s :: Type) where
    {-# MINIMAL offset #-}

    {- | Parser returning the current stream offset. -}
    offset :: ParserT s Void m Word
