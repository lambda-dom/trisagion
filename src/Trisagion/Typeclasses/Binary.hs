{- |
Module: Trisagion.Typeclasses.Binary

The @Binary@ for parsers with constraints @'Splittable' m Word8 b s@.
-}

module Trisagion.Typeclasses.Binary (
    -- * Typeclasses.
    Binary (..),

    -- * Generic parsers.
    integralLe,
    integralBe,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Int (Int8)
import Data.Word (Word8, Word16, Word32, Word64)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Utils.Bits (byteCount, pack, packReverse)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Streamable (InputError, one)
import Trisagion.Parsers.Splittable (takeExact)


{- | The @Binary@ typeclass for efficient parsers for machine-width types. -}
class (Splittable Word8 b s, MonoFoldable Word8 b) => Binary b s where
    {- | Parse a single 'Word8'. -}
    word8 :: Parser s InputError Word8
    word8 = one

    {- | Parse a single 'Int8'. -}
    int8 :: Parser s InputError Int8
    int8 = fromIntegral <$> word8

    {- | Parse a 'Word16' in little-endian format. -}
    word16Le :: Parser s InputError Word16
    word16Le = integralLe

    {- | Parse a 'Word32' in little-endian format. -}
    word32Le :: Parser s InputError Word32
    word32Le = integralLe

    {- | Parse a 'Word64' in little-endian format. -}
    word64Le :: Parser s InputError Word64
    word64Le = integralLe

    {- | Parse a 'Word16' in big-endian format. -}
    word16Be :: Parser s InputError Word16
    word16Be = integralBe

    {- | Parse a 'Word32' in big-endian format. -}
    word32Be :: Parser s InputError Word32
    word32Be = integralBe

    {- | Parse a 'Word64' in big-endian format. -}
    word64Be :: Parser s InputError Word64
    word64Be = integralBe


{- | Parse a machine-width integral in little-endian format. -}
{-# INLINEABLE integralLe #-}
integralLe
    :: forall b s w
    .  (Splittable Word8 b s, MonoFoldable Word8 b, Integral w, FiniteBits w)
    => Parser s InputError w
integralLe = do
        bs <- takeExact $ byteCount @w 0
        pure $ pack (monotoList bs)

{- | Parse a machine-width integral in big-endian format. -}
{-# INLINEABLE integralBe #-}
integralBe
    :: forall b s w
    .  (Splittable Word8 b s, MonoFoldable Word8 b, Integral w, FiniteBits w)
    => Parser s InputError w
integralBe = do
        s <- takeExact $ byteCount @w 0
        pure $ packReverse (monotoList s)
