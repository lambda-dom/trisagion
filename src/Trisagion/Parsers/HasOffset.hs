{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'HasOffset' s@ constraints.
-}

module Trisagion.Parsers.HasOffset (
    -- * Parsers @'HasOffset' s => 'Parser' s e a@.
    offset,
) where

-- Imports.
-- Base.
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (gets)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset)
import qualified Trisagion.Typeclasses.HasOffset as HasOffset (offset)
import Trisagion.Parser (Parser)


{- | Parser returning the current offset of the input stream. -}
{-# INLINE offset #-}
offset :: HasOffset s => Parser s Void Int
offset = gets HasOffset.offset
