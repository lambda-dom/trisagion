{- |
Module: Trisagion.Parsers.Word8

Parsers with constraints @'Streamable' m Word8 s@.
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
import Data.Word (Word8, Word16, Word32, Word64)
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

{- | Parse a 'Word16' in little-endian format. -}
{-# INLINE word16Le #-}
word16Le
    :: forall m b s
    .  (Splittable m Word8 b s, MonoFoldable Word8 b)
    => ParserT s InputError m Word16
word16Le = integralLe

{- | Parse a 'Word32' in little-endian format. -}
{-# INLINE word32Le #-}
word32Le
    :: forall m b s
    .  (Splittable m Word8 b s, MonoFoldable Word8 b)
    => ParserT s InputError m Word32
word32Le = integralLe

{- | Parse a 'Word64' in little-endian format. -}
{-# INLINE word64Le #-}
word64Le
    :: forall m b s
    .  (Splittable m Word8 b s, MonoFoldable Word8 b)
    => ParserT s InputError m Word64
word64Le = integralLe

{- | Parse a 'Word16' in big-endian format. -}
{-# INLINE word16Be #-}
word16Be
    :: forall m b s
    .  (Splittable m Word8 b s, MonoFoldable Word8 b)
    => ParserT s InputError m Word16
word16Be = integralBe

{- | Parse a 'Word32' in big-endian format. -}
{-# INLINE word32Be #-}
word32Be
    :: forall m b s
    .  (Splittable m Word8 b s, MonoFoldable Word8 b)
    => ParserT s InputError m Word32
word32Be = integralBe

{- | Parse a 'Word64' in big-endian format. -}
{-# INLINE word64Be #-}
word64Be
    :: forall m b s
    .  (Splittable m Word8 b s, MonoFoldable Word8 b)
    => ParserT s InputError m Word64
word64Be = integralBe
