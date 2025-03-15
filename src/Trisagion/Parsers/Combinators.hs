{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * Parsers without errors.
    value,
) where

-- Imports.
-- Base.
import Data.Void (Void)

-- Package.
import Trisagion.Parser (Parser)


{- | Embed a value in the t'Parser' monad.

The difference with 'pure' from 'Applicative' is the more precise signature.
-}
value :: a -> Parser s Void a
value = pure
