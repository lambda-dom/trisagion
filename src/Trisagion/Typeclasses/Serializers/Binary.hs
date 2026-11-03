{- |
Module: Trisagion.Typeclasses.Serializers.Binary

The @Binary@ typeclass for serializers with @'Sink' 'Word8' b s@ constraints.
-}

module Trisagion.Typeclasses.Serializers.Binary (
    -- * Typeclasses.
    Binary (..),

    -- Integral, primitive serializers.
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes (unpack)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder (
    word8,
    word16LE,
    word32LE,
    word64LE,
    word16BE,
    word32BE,
    word64BE,
    byteString,
    shortByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short (unpack)

-- Package.
import Trisagion.Typeclasses.Sink (Sink, single)
import Trisagion.Serializer (Serializer, embed)
import Trisagion.Serializers.Combinators (many)


{- | The @Binary@ typeclass for efficient serializers for machine-width types. -}
class Sink Word8 b s => Binary b s where
    {-# MINIMAL word16Le, word32Le, word64Le, word16Be, word32Be, word64Be  #-}

    {- | Serialize a single 'Word8'. -}
    {-# INLINE word8 #-}
    word8 :: Serializer s Word8
    word8 = embed single

    {- | Serialize a 'Word16' in little-endian format. -}
    word16Le :: Serializer s Word16

    {- | Serialize a 'Word32' in little-endian format. -}
    word32Le :: Serializer s Word32

    {- | Serialize a 'Word64' in little-endian format. -}
    word64Le :: Serializer s Word64

    {- | Serialize a 'Word16' in big-endian format. -}
    word16Be :: Serializer s Word16

    {- | Serialize a 'Word32' in big-endian format. -}
    word32Be :: Serializer s Word32

    {- | Serialize a 'Word64' in big-endian format. -}
    word64Be :: Serializer s Word64

    {- | Serialize a 'Data.ByteString.ByteString'. -}
    {-# INLINE bytes #-}
    bytes :: Serializer s ByteString
    bytes = contramap Bytes.unpack (many word8)

    {- | Serialize a 'Data.ByteString.ByteString'. -}
    {-# INLINE shortBytes #-}
    shortBytes :: Serializer s ShortByteString
    shortBytes = contramap Short.unpack (many word8)


-- Instances.
instance Binary Lazy.ByteString Builder where
    {-# INLINE word8 #-}
    word8 :: Serializer Builder Word8
    word8 = embed $ Builder.word8

    {-# INLINE word16Le #-}
    word16Le :: Serializer Builder Word16
    word16Le = embed $ Builder.word16LE

    {-# INLINE word32Le #-}
    word32Le :: Serializer Builder Word32
    word32Le = embed $ Builder.word32LE

    {-# INLINE word64Le #-}
    word64Le :: Serializer Builder Word64
    word64Le = embed $ Builder.word64LE

    {-# INLINE word16Be #-}
    word16Be :: Serializer Builder Word16
    word16Be = embed $ Builder.word16BE

    {-# INLINE word32Be #-}
    word32Be :: Serializer Builder Word32
    word32Be = embed $ Builder.word32BE

    {-# INLINE word64Be #-}
    word64Be :: Serializer Builder Word64
    word64Be = embed $ Builder.word64BE

    {- | Serialize a 'Data.ByteString.ByteString'. -}
    {-# INLINE bytes #-}
    bytes :: Serializer Builder ByteString
    bytes = embed $ Builder.byteString

    {- | Serialize a 'Data.ByteString.Short.ShortByteString'. -}
    {-# INLINE shortBytes #-}
    shortBytes :: Serializer Builder ShortByteString
    shortBytes = embed $ Builder.shortByteString
