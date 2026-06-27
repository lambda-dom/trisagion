{- |
Module: Trisagion.Serializers.Binary

The @Binary@ typeclass for serializers with constraints @'Sink' Word8 b s@.
-}

module Trisagion.Serializers.Binary (
    -- * Typeclasses.
    Binary (..),

    -- * Generic serializers.
    integralLe,
    integralBe,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Bytes (word8, int8, word16LE, word32LE, word64LE, word16BE, word32BE, word64BE)

-- Package.
import Trisagion.Utils.Bits (unpack, unpackReverse)
import Trisagion.Typeclasses.Sink (Sink (..))
import Trisagion.Serializer (Serializer, embed)


{- | The @Binary@ typeclass for efficient serializers for machine-width types. -}
class Sink Word8 b s => Binary b s where
    {- | Serialize a single 'Word8'. -}
    word8 :: Serializer s Word8
    word8 = embed single

    {- | Serialize a single 'Int8'. -}
    int8 :: Serializer s Int8
    int8 = contramap fromIntegral word8

    {- | Serialize a 'Word16' in little-endian format. -}
    word16Le :: Serializer s Word16
    word16Le = integralLe

    {- | Serialize a 'Word32' in little-endian format. -}
    word32Le :: Serializer s Word32
    word32Le = integralLe

    {- | Serialize a 'Word64' in little-endian format. -}
    word64Le :: Serializer s Word64
    word64Le = integralLe

    {- | Serialize a 'Word16' in big-endian format. -}
    word16Be :: Serializer s Word16
    word16Be = integralBe

    {- | Serialize a 'Word32' in big-endian format. -}
    word32Be :: Serializer s Word32
    word32Be = integralBe

    {- | Serialize a 'Word64' in big-endian format. -}
    word64Be :: Serializer s Word64
    word64Be = integralBe


-- Instances.
instance Binary ByteString Builder where
    {-# INLINE word8 #-}
    word8 :: Serializer Builder Word8
    word8 = embed $ Bytes.word8

    {-# INLINE int8 #-}
    int8 :: Serializer Builder Int8
    int8 = embed $ Bytes.int8

    {-# INLINE word16Le #-}
    word16Le :: Serializer Builder Word16
    word16Le = embed $ Bytes.word16LE

    {-# INLINE word32Le #-}
    word32Le :: Serializer Builder Word32
    word32Le = embed $ Bytes.word32LE

    {-# INLINE word64Le #-}
    word64Le :: Serializer Builder Word64
    word64Le = embed $ Bytes.word64LE

    {-# INLINE word16Be #-}
    word16Be :: Serializer Builder Word16
    word16Be = embed $ Bytes.word16BE

    {-# INLINE word32Be #-}
    word32Be :: Serializer Builder Word32
    word32Be = embed $ Bytes.word32BE

    {-# INLINE word64Be #-}
    word64Be :: Serializer Builder Word64
    word64Be = embed $ Bytes.word64BE


{- | Serialize machine-width integral in little-endian format. -}
{-# INLINE integralLe #-}
integralLe :: (Sink Word8 b s, Integral a, FiniteBits a) => Serializer s a
integralLe = embed $ many . unpack

{- | Serialize machine-width integral in big-endian format. -}
{-# INLINE integralBe #-}
integralBe :: (Sink Word8 b s, Integral a, FiniteBits a) => Serializer s a
integralBe = embed $ many . unpackReverse
