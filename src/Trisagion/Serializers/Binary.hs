{- |
Module: Trisagion.Serializers.Binary

Serializers @'Binary' m => 'Serializer' m a@.
-}

module Trisagion.Serializers.Binary (
    -- * Serializers @'Binary' m => 'Serializer' m a@.
    word8,
    int8,

    -- * Little-endian serializers.
    word16Le,
    word32Le,
    word64Le,
    int16Le,
    int32Le,
    int64Le,

    -- * Big-endian serializers.
    word16Be,
    word32Be,
    word64Be,
    int16Be,
    int32Be,
    int64Be,

    -- * Text to binary serializers.
    char,
    string,

    -- * 'ByteString' serializers.
    bytestring,
) where

-- Imports.
-- Base.
import Data.Char (ord)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Data.ByteString (ByteString)

-- Package.
import Trisagion.Typeclasses.Builder (one)
import Trisagion.Typeclasses.Binary (Binary)
import qualified Trisagion.Typeclasses.Binary as Binary (
    word16Le,
    word32Le,
    word64Le,
    word16Be,
    word32Be,
    word64Be,
    int16Le,
    int32Le,
    int64Le,
    int16Be,
    int32Be,
    int64Be,
    bytestring,)
import Trisagion.Serializer (Serializer, embed)
import Trisagion.Serializers.Combinators (listOf)


{- | Serialize a 'Word8'. -}
{-# INLINE word8 #-}
word8 :: Binary m => Serializer m Word8
word8 = embed one

{- | Serialize an 'Int8'. -}
{-# INLINE int8 #-}
int8 :: Binary m => Serializer m Int8
int8 = contramap fromIntegral word8

{- | Serialize a 'Word16' in little-endian format. -}
{-# INLINE word16Le #-}
word16Le :: Binary m => Serializer m Word16
word16Le = embed Binary.word16Le

{- | Serialize a 'Word32' in little-endian format. -}
{-# INLINE word32Le #-}
word32Le :: Binary m => Serializer m Word32
word32Le = embed Binary.word32Le

{- | Serialize a 'Word64' in little-endian format. -}
{-# INLINE word64Le #-}
word64Le :: Binary m => Serializer m Word64
word64Le = embed Binary.word64Le

{- | Serialize a 'Int16' in little-endian format. -}
{-# INLINE int16Le #-}
int16Le :: Binary m => Serializer m Int16
int16Le = embed Binary.int16Le

{- | Serialize a 'Int32' in little-endian format. -}
{-# INLINE int32Le #-}
int32Le :: Binary m => Serializer m Int32
int32Le = embed Binary.int32Le

{- | Serialize a 'Int64' in little-endian format. -}
{-# INLINE int64Le #-}
int64Le :: Binary m => Serializer m Int64
int64Le = embed Binary.int64Le

{- | Serialize a 'Word16' in big-endian format. -}
{-# INLINE word16Be #-}
word16Be :: Binary m => Serializer m Word16
word16Be = embed Binary.word16Be

{- | Serialize a 'Word32' in big-endian format. -}
{-# INLINE word32Be #-}
word32Be :: Binary m => Serializer m Word32
word32Be = embed Binary.word32Be

{- | Serialize a 'Word64' in big-endian format. -}
{-# INLINE word64Be #-}
word64Be :: Binary m => Serializer m Word64
word64Be = embed Binary.word64Be

{- | Serialize a 'Int16' in big-endian format. -}
{-# INLINE int16Be #-}
int16Be :: Binary m => Serializer m Int16
int16Be = embed Binary.int16Be

{- | Serialize a 'Int32' in big-endian format. -}
{-# INLINE int32Be #-}
int32Be :: Binary m => Serializer m Int32
int32Be = embed Binary.int32Be

{- | Serialize a 'Int64' in big-endian format. -}
{-# INLINE int64Be #-}
int64Be :: Binary m => Serializer m Int64
int64Be = embed Binary.int64Be


{- | Serialize a 'Char'.

Truncates 'Char' to 'Word8'. It is the responsability of the caller to sanitize the input.
-}
{-# INLINE char #-}
char :: Binary m => Serializer m Char
char =  contramap (fromIntegral . ord) word8

{- | Serialize a 'String'.

Truncates 'Char' to 'Word8'. It is the responsability of the caller to sanitize the input.
-}
{-# INLINE string #-}
string :: Binary m => Serializer m String
string = listOf char


{- | Serialize a (strict) 'ByteString'. -}
{-# INLINE bytestring #-}
bytestring :: Binary m => Serializer m ByteString
bytestring = embed Binary.bytestring
