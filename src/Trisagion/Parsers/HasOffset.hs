{- |
Module: Trisagion.Parsers.HasOffset

Parsers @'HasOffset' s => 'Parser' s@.
-}

module Trisagion.Parsers.HasOffset (
    -- * Parsers @'HasOffset' s => 'Parser' s@.
    consumed,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (absurd)

-- Package.
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Parser (Parser, get)


{- | Run the parser and return its result along with the prefix of consumed input. -}
{-# INLINE consumed #-}
consumed :: (Splittable s, HasOffset s) => Parser s e a -> Parser s e (PrefixOf s, a)
consumed p = do
    xs <- first absurd get
    x  <- p
    n  <- offset <$> first absurd get
    let size = n - offset xs
    pure (fst $ splitPrefix size xs, x)
