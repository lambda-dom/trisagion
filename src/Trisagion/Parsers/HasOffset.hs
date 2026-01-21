{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'HasOffset' m s@ constraints.
-}

module Trisagion.Parsers.HasOffset (
    -- * Parsers @'HasOffset' m s => 'ParserT' m s e a@.
    offset,
) where

-- Imports.
-- Base.
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset)
import qualified Trisagion.Typeclasses.HasOffset as HasOffset (offset)
import Trisagion.ParserT (ParserT)


{- | Parser returning the current stream offset. -}
{-# INLINE offset #-}
offset :: (HasOffset m s, Monad m) => ParserT s Void m Word
offset = get >>= lift . HasOffset.offset
