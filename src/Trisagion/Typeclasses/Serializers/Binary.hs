{- |
Module: Trisagion.Typeclasses.Serializers.Binary

The @Binary@ typeclass for serializers with @'Sink' 'Word8' b s@ constraints.
-}

module Trisagion.Typeclasses.Serializers.Binary (
    -- * Typeclasses.
    Binary (..),

    -- * Serializers.
    byteString,
    shortByteString,
    latin1,
    char,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Bytes (word8, int8, word16LE, word32LE, word64LE, word16BE, word32BE, word64BE, byteString, shortByteString, char8, charUtf8)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString.Short (ShortByteString)
import Optics.Core (view, review)

-- non-Hackage libraries.
import Data.Int.Optics (int8ToWord8)

-- Package.
import Trisagion.Typeclasses.Sink (Sink, single)
import Trisagion.Serializer (Serializer, embed)
import Data.Char (chr)
import Data.Word.Optics (word8ToInt)


{- | The @Binary@ typeclass for efficient serializers for machine-width types. -}
class Sink Word8 b s => Binary b s where
    {-# MINIMAL word16Le, word32Le, word64Le, word16Be, word32Be, word64Be #-}

    {- | Serialize a single 'Word8'. -}
    {-# INLINE word8 #-}
    word8 :: Serializer s Word8
    word8 = embed single

    {- | Serialize a single 'Int8'. -}
    {-# INLINE int8 #-}
    int8 :: Serializer s Int8
    int8 = contramap (view int8ToWord8) word8

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


-- Instances.
instance Binary Lazy.ByteString Builder where
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


{- | Serialize a 'Data.ByteString.ByteString'. -}
{-# INLINE byteString #-}
byteString :: Serializer Builder ByteString
byteString = embed $ Bytes.byteString

{- | Serialize a 'Data.ByteString.Short.ShortByteString'. -}
{-# INLINE shortByteString #-}
shortByteString :: Serializer Builder ShortByteString
shortByteString = embed $ Bytes.shortByteString

{- | Serialize a 'Word8' in the latin-1, or ISO/IEC 8859-1, encoding.

The latin-1 encoding is a superset of ascii, so this Serializer can also be used to serialize 'Word8'
in the ascii encoding as long as it is known that it is in the range @[0 .. 127]@. If the argument
is not in this range, it is truncated to fit.
-}
{-# INLINE latin1 #-}
latin1 :: Serializer Builder Word8
latin1 = embed $ Bytes.char8 . chr . (review word8ToInt)

{- | Serialize a 'Char' in the utf8 encoding. -}
{-# INLINE char #-}
char :: Serializer Builder Char
char = embed $ Bytes.charUtf8
