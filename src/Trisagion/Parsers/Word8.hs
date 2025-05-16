{- |
Module: Trisagion.Parsers.Word8

Parsers @('Streamable' s, 'ElementOf' s ~ Word8) => 'Parser' s@.
-}

module Trisagion.Parsers.Word8 (
    -- * Atomic parsers.
    word8,
    int8,
) where

-- Imports.
-- Base.
import Data.Int (Int8)
import Data.Word (Word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Parser (Parser, InputError, one)


{- | Parse a single 'Word8'. -}
word8 :: (Streamable s, ElementOf s ~ Word8) => Parser s (InputError s) Word8
word8 = one

{- | Parse a single 'Int8'. -}
int8 :: (Streamable s, ElementOf s ~ Word8) => Parser s (InputError s) Int8
int8 = fromIntegral <$> one

