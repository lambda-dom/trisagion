{- |
Module: Trisagion.Parsers.Word8

Parsers @('Streamable' s, 'ElementOf' s ~ Word8) => 'Parser' s@.
-}

module Trisagion.Parsers.Word8 (
    -- * Atomic parsers.
    word8,
    int8,

    -- * Generic parsers.
    integralLe,
    integralBe,

    -- ** Specializations.
    word16Le,
    word32Le,
    word64Le,
    word16Be,
    word32Be,
    word64Be,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Int (Int8)
import Data.Word (Word8, Word16, Word32, Word64)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Mono.Types.ByteArray (byteCount, pack, packReverse)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser


{- | Parse a single 'Word8'. -}
{-# INLINE word8 #-}
word8 :: (Streamable s, ElementOf s ~ Word8) => Parser s InputError Word8
word8 = one

{- | Parse a single 'Int8'. -}
{-# INLINE int8 #-}
int8 :: (Streamable s, ElementOf s ~ Word8) => Parser s InputError Int8
int8 = fromIntegral <$> one


{- | Parse a machine-width integral in little-endian format. -}
{-# INLINEABLE integralLe #-}
integralLe
    :: forall s w
    .  (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8, Integral w, FiniteBits w)
    => Parser s InputError w
integralLe = do
        s <- takeExact $ byteCount @w 0
        pure $ pack (monotoList s)

{- | Parse a machine-width integral in big-endian format. -}
{-# INLINEABLE integralBe #-}
integralBe
    :: forall s w
    .  (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8, Integral w, FiniteBits w)
    => Parser s InputError w
integralBe = do
        s <- takeExact $ byteCount @w 0
        pure $ packReverse (monotoList s)


{- | Parse a 'Word16' in little-endian format. -}
{-# INLINE word16Le #-}
word16Le
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Word16
word16Le = integralLe

{- | Parse a 'Word32' in little-endian format. -}
{-# INLINE word32Le #-}
word32Le
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Word32
word32Le = integralLe

{- | Parse a 'Word64' in little-endian format. -}
{-# INLINE word64Le #-}
word64Le
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Word64
word64Le = integralLe

{- | Parse a 'Word16' in big-endian format. -}
{-# INLINE word16Be #-}
word16Be
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Word16
word16Be = integralBe

{- | Parse a 'Word32' in big-endian format. -}
{-# INLINE word32Be #-}
word32Be
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Word32
word32Be = integralBe

{- | Parse a 'Word64' in big-endian format. -}
{-# INLINE word64Be #-}
word64Be
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Word64
word64Be = integralBe
