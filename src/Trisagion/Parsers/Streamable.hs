{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Streamable (
    -- * Parsers @'Streamable' s => 'Parser' s e a@.
    eoi,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (null)

-- Base.
import Data.Void (Void)

-- Package.
import Trisagion.Parser (Parser, get)
import Trisagion.Typeclasses.Streamable (Streamable (..))


{- | Return @'True'@ if all input is consumed. -}
eoi :: Streamable s => Parser s Void Bool
eoi = null <$> get

