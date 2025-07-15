{- |
Module: Trisagion.Typeclasses.Binary

The @Binary@ typeclass for binary builders.
-}

module Trisagion.Typeclasses.Binary (
    -- * Typeclasses.
    Binary (..),
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Data.ByteString (ByteString, unpack)
import Data.ByteString.Builder (
    word16LE,
    word32LE,
    word64LE,
    word16BE,
    word32BE,
    word64BE,
    int16LE,
    int32LE,
    int64LE,
    int16BE,
    int32BE,
    int64BE,
    byteString)
import qualified Data.ByteString.Builder as Bytes (Builder, int8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Types.ByteArray (bytes, bytesReverse)

-- Package.
import Trisagion.Typeclasses.Builder (Builder, BuilderOf, many, one)


{- | The @Binary@ typeclass for binary builders.

This class is used for optimization purposes only, as it does not add any new operations that
cannot be done with the parent superclass.
-}
class (Builder m, ElementOf (BuilderOf m) ~ Word8) => Binary m where
    {- | Serialize an integral number in little-endian format. -}
    wordLe :: (Integral a, FiniteBits a) => a -> m
    wordLe = many . bytes

    {- | Specialization of 'wordLe' to 'Word16', -}
    word16Le :: Word16 -> m
    word16Le = wordLe

    {- | Specialization of 'wordLe' to 'Word32', -}
    word32Le :: Word32 -> m
    word32Le = wordLe

    {- | Specialization of 'wordLe' to 'Word64', -}
    word64Le :: Word64 -> m
    word64Le = wordLe

    {- | Serialize an integral number in big-endian format. -}
    wordBe :: (Integral a, FiniteBits a) => a -> m
    wordBe = many . bytesReverse

    {- | Specialization of 'wordBe' to 'Word16', -}
    word16Be :: Word16 -> m
    word16Be = wordLe

    {- | Specialization of 'wordBe' to 'Word32', -}
    word32Be :: Word32 -> m
    word32Be = wordBe

    {- | Specialization of 'wordBe' to 'Word64', -}
    word64Be :: Word64 -> m
    word64Be = wordBe

    {- | Serialize an 'Int8'. -}
    int8 :: Int8 -> m
    int8 = one . fromIntegral

    {- | Serialize an 'Int16' in little-endian format. -}
    int16Le :: Int16 -> m
    int16Le = word16Le . fromIntegral

    {- | Serialize an 'Int32' in little-endian format. -}
    int32Le :: Int32 -> m
    int32Le = word32Le . fromIntegral

    {- | Serialize an 'Int64' in little-endian format. -}
    int64Le :: Int64 -> m
    int64Le = word64Le . fromIntegral

    {- | Serialize an 'Int16' in big-endian format. -}
    int16Be :: Int16 -> m
    int16Be = word16Be . fromIntegral

    {- | Serialize an 'Int32' in big-endian format. -}
    int32Be :: Int32 -> m
    int32Be = word32Be . fromIntegral

    {- | Serialize an 'Int64' in big-endian format. -}
    int64Be :: Int64 -> m
    int64Be = word64Be . fromIntegral

    {- | Serialize a (strict) 'ByteString'. -}
    bytestring :: ByteString -> m
    bytestring = foldMap one . unpack

-- Instances.
instance Binary Bytes.Builder where
    {-# INLINE word16Le #-}
    word16Le = word16LE

    {-# INLINE word32Le #-}
    word32Le = word32LE

    {-# INLINE word64Le #-}
    word64Le = word64LE

    {-# INLINE word16Be #-}
    word16Be = word16BE

    {-# INLINE word32Be #-}
    word32Be = word32BE

    {-# INLINE word64Be #-}
    word64Be = word64BE

    {-# INLINE int8 #-}
    int8 = Bytes.int8

    {-# INLINE int16Le #-}
    int16Le = int16LE

    {-# INLINE int32Le #-}
    int32Le = int32LE

    {-# INLINE int64Le #-}
    int64Le = int64LE

    {-# INLINE int16Be #-}
    int16Be = int16BE

    {-# INLINE int32Be #-}
    int32Be = int32BE

    {-# INLINE int64Be #-}
    int64Be = int64BE

    {-# INLINE bytestring #-}
    bytestring = byteString
