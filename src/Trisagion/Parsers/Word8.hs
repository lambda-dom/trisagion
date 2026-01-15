{- |
Module: Trisagion.Parsers.Word8

Parsers with constraints @'Streamable' m Word8 s@.
-}

module Trisagion.Parsers.Word8 (
    -- * Atomic parsers.
    word8,
    int8,

    -- Generic parsers.
    integralLe,
    integralBe,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Word (Word8)
import Data.Int (Int8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Utils.Integral (byteCount, pack, packReverse)
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable)
import Trisagion.ParserT (ParserT)
import Trisagion.Parsers.Streamable (InputError, headP)
import Trisagion.Parsers.Splittable (takeExact)


{- | Parse a single 'Word8'. -}
{-# INLINE word8 #-}
word8 :: Streamable m Word8 s => ParserT s InputError m Word8
word8 = headP

{- | Parse a single 'Int8'. -}
{-# INLINE int8 #-}
int8 :: Streamable m Word8 s => ParserT s InputError m Int8
int8 = fromIntegral <$> word8

{- | Parse a machine-width integral in little-endian format. -}
{-# INLINEABLE integralLe #-}
integralLe
    :: forall m b s w
    .  (Splittable m Word8 b s, MonoFoldable Word8 b, Integral w, FiniteBits w)
    => ParserT s InputError m w
integralLe = do
        bs <- takeExact $ byteCount @w 0
        pure $ pack (monotoList bs)

{- | Parse a machine-width integral in big-endian format. -}
{-# INLINEABLE integralBe #-}
integralBe
    :: forall m b s w
    .  (Splittable m Word8 b s, MonoFoldable Word8 b, Integral w, FiniteBits w)
    => ParserT s InputError m w
integralBe = do
        s <- takeExact $ byteCount @w 0
        pure $ packReverse (monotoList s)
