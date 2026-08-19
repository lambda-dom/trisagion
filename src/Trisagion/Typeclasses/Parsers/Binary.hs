{- |
Module: Trisagion.Typeclasses.Parsers.Binary

The @Binary@ typeclass for parsers with @'Split' 'Word8' b s@ constraints.
-}

module Trisagion.Typeclasses.Parsers.Binary (
    -- * Typeclasses.
    Binary (..),
) where

-- Imports.
-- Base.
import Data.Int (Int8)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, index)
import qualified Data.ByteString.Lazy as LBytes (ByteString, index)
import qualified Data.ByteString.Short as SBytes (ShortByteString, index)
import Optics.Core (review, view)

-- non-Hackage libraries.
import Data.Bits.Optics (word16BytesLe, word32BytesLe, word64BytesLe, word16BytesBe, word32BytesBe, word64BytesBe)
import Data.Word.Optics (word8ToInt8)

-- Package.
import Trisagion.Typeclasses.Split (Split)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError, one)
import Trisagion.Parsers.Split (takeExact)


-- $setup
-- >>> import Trisagion.Parser


-- Private module functions.
{-# INLINE mkParserWord16Le #-}
mkParserWord16Le :: (Split Word8 b s) => (b -> (Word8, Word8)) -> Parser s InputError Word16
mkParserWord16Le f = (review word16BytesLe . f) <$> takeExact 2

{-# INLINE mkParserWord32Le #-}
mkParserWord32Le
    :: (Split Word8 b s)
    => (b -> (Word8, Word8, Word8, Word8)) -> Parser s InputError Word32
mkParserWord32Le f = (review word32BytesLe . f) <$> takeExact 4

{-# INLINE mkParserWord64Le #-}
mkParserWord64Le
    :: (Split Word8 b s)
    => (b -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)) -> Parser s InputError Word64
mkParserWord64Le f = (review word64BytesLe . f) <$> takeExact 8

{-# INLINE mkParserWord16Be #-}
mkParserWord16Be :: (Split Word8 b s) => (b -> (Word8, Word8)) -> Parser s InputError Word16
mkParserWord16Be f = (review word16BytesBe . f) <$> takeExact 2

{-# INLINE mkParserWord32Be #-}
mkParserWord32Be
    :: (Split Word8 b s)
    => (b -> (Word8, Word8, Word8, Word8)) -> Parser s InputError Word32
mkParserWord32Be f = (review word32BytesBe . f) <$> takeExact 4

{-# INLINE mkParserWord64Be #-}
mkParserWord64Be
    :: (Split Word8 b s)
    => (b -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)) -> Parser s InputError Word64
mkParserWord64Be f = (review word64BytesBe . f) <$> takeExact 8


{- | The @Binary@ typeclass for efficient parsers for machine-width types. -}
class (Split Word8 b s) => Binary b s where
    {-# MINIMAL word16Le, word32Le, word64Le, word16Be, word32Be, word64Be #-}

    {- | Parse a single 'Word8'. -}
    {-# INLINE word8 #-}
    word8 :: Parser s InputError Word8
    word8 = one

    {- | Parse a single 'Int8'. -}
    {-# INLINE int8 #-}
    int8 :: Parser s InputError Int8
    int8 = (view word8ToInt8) <$> word8

    {- | Parse a 'Word16' in little-endian format. -}
    word16Le :: Parser s InputError Word16

    {- | Parse a 'Word32' in little-endian format. -}
    word32Le :: Parser s InputError Word32

    {- | Parse a 'Word64' in little-endian format.-}
    word64Le :: Parser s InputError Word64

    {- | Parse a 'Word16' in big-endian format. -}
    word16Be :: Parser s InputError Word16

    {- | Parse a 'Word32' in big-endian format. -}
    word32Be :: Parser s InputError Word32

    {- | Parse a 'Word64' in big-endian format. -}
    word64Be :: Parser s InputError Word64


-- Instances.
instance Binary [Word8] [Word8] where
    {-# INLINE word16Le #-}
    word16Le :: Parser [Word8] InputError Word16
    word16Le = mkParserWord16Le h
        where
            h :: [Word8] -> (Word8, Word8)
            h (m : n : _) = (m, n)
            h _           = error "Impossible case."

    {- | Parse a 'Word32' in little-endian format.

    === __Examples:__

    >>> parse word32Le [1, 0, 0, 0, 0, 0, 0, 0]
    Right (1,[0,0,0,0])

    >>> parse word32Le [0, 1, 0, 0, 0, 0, 0, 0]
    Right (256,[0,0,0,0])

    >>> parse word32Le [0, 0, 1, 0, 0, 0, 0, 0]
    Right (65536,[0,0,0,0])

    >>> parse word32Le [0, 0, 0, 1, 0, 0, 0, 0]
    Right (16777216,[0,0,0,0])
    -}
    {-# INLINE word32Le #-}
    word32Le :: Parser [Word8] InputError Word32
    word32Le = mkParserWord32Le h
        where
            h :: [Word8] -> (Word8, Word8, Word8, Word8)
            h (m : n : p : q : _) = (m, n, p, q)
            h _                   = error "Impossible case."

    {-# INLINE word64Le #-}
    word64Le :: Parser [Word8] InputError Word64
    word64Le = mkParserWord64Le h
        where
            h :: [Word8] -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
            h (m : n : p : q : r : s : t : u : _) = (m, n, p, q, r, s, t, u)
            h _                                   = error "Impossible case."

    {-# INLINE word16Be #-}
    word16Be :: Parser [Word8] InputError Word16
    word16Be = mkParserWord16Be h
        where
            h :: [Word8] -> (Word8, Word8)
            h (m : n : _) = (n, m)
            h _           = error "Impossible case."

    {- | Parse a 'Word32' in big-endian format.

    === __Examples:__

    >>> parse word32Be [0, 0, 0, 1, 0, 0, 0, 0]
    Right (1,[0,0,0,0])

    >>> parse word32Be [0, 0, 1, 0, 0, 0, 0, 0]
    Right (256,[0,0,0,0])

    >>> parse word32Be [0, 1, 0, 0, 0, 0, 0, 0]
    Right (65536,[0,0,0,0])

    >>> parse word32Be [1, 0, 0, 0, 0, 0, 0, 0]
    Right (16777216,[0,0,0,0])
    -}
    {-# INLINE word32Be #-}
    word32Be :: Parser [Word8] InputError Word32
    word32Be = mkParserWord32Be h
        where
            h :: [Word8] -> (Word8, Word8, Word8, Word8)
            h (m : n : p : q : _) = (q, p, n, m)
            h _                   = error "Impossible case."

    {-# INLINE word64Be #-}
    word64Be :: Parser [Word8] InputError Word64
    word64Be = mkParserWord64Be h
        where
            h :: [Word8] -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
            h (m : n : p : q : r : s : t : u : _) = (u, t, s, r, q, p, n, m)
            h _                                   = error "Impossible case."

instance Binary Bytes.ByteString Bytes.ByteString where
    {-# INLINE word16Le #-}
    word16Le :: Parser Bytes.ByteString InputError Word16
    word16Le = mkParserWord16Le h
        where
            h :: Bytes.ByteString -> (Word8, Word8)
            h xs = (Bytes.index xs 0, Bytes.index xs 1)

    {-# INLINE word32Le #-}
    word32Le :: Parser Bytes.ByteString InputError Word32
    word32Le = mkParserWord32Le h
        where
            h :: Bytes.ByteString -> (Word8, Word8, Word8, Word8)
            h xs = (Bytes.index xs 0, Bytes.index xs 1, Bytes.index xs 2, Bytes.index xs 3)

    {-# INLINE word64Le #-}
    word64Le :: Parser Bytes.ByteString InputError Word64
    word64Le = mkParserWord64Le h
        where
            h :: Bytes.ByteString -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
            h xs =
                (
                    Bytes.index xs 0,
                    Bytes.index xs 1,
                    Bytes.index xs 2,
                    Bytes.index xs 3,
                    Bytes.index xs 4,
                    Bytes.index xs 5,
                    Bytes.index xs 6,
                    Bytes.index xs 7
                )

    {-# INLINE word16Be #-}
    word16Be :: Parser Bytes.ByteString InputError Word16
    word16Be = mkParserWord16Be h
        where
            h :: Bytes.ByteString -> (Word8, Word8)
            h xs = (Bytes.index xs 1, Bytes.index xs 0)

    {-# INLINE word32Be #-}
    word32Be :: Parser Bytes.ByteString InputError Word32
    word32Be = mkParserWord32Be h
        where
            h :: Bytes.ByteString -> (Word8, Word8, Word8, Word8)
            h xs = (Bytes.index xs 3, Bytes.index xs 2, Bytes.index xs 1, Bytes.index xs 0)

    {-# INLINE word64Be #-}
    word64Be :: Parser Bytes.ByteString InputError Word64
    word64Be = mkParserWord64Be h
        where
            h :: Bytes.ByteString -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
            h xs =
                (
                    Bytes.index xs 7,
                    Bytes.index xs 6,
                    Bytes.index xs 5,
                    Bytes.index xs 4,
                    Bytes.index xs 3,
                    Bytes.index xs 2,
                    Bytes.index xs 1,
                    Bytes.index xs 0
                )

instance Binary LBytes.ByteString LBytes.ByteString where
    {-# INLINE word16Le #-}
    word16Le :: Parser LBytes.ByteString InputError Word16
    word16Le = mkParserWord16Le h
        where
            h :: LBytes.ByteString -> (Word8, Word8)
            h xs = (LBytes.index xs 0, LBytes.index xs 1)

    {-# INLINE word32Le #-}
    word32Le :: Parser LBytes.ByteString InputError Word32
    word32Le = mkParserWord32Le h
        where
            h :: LBytes.ByteString -> (Word8, Word8, Word8, Word8)
            h xs = (LBytes.index xs 0, LBytes.index xs 1, LBytes.index xs 2, LBytes.index xs 3)

    {-# INLINE word64Le #-}
    word64Le :: Parser LBytes.ByteString InputError Word64
    word64Le = mkParserWord64Le h
        where
            h :: LBytes.ByteString -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
            h xs =
                (
                    LBytes.index xs 0,
                    LBytes.index xs 1,
                    LBytes.index xs 2,
                    LBytes.index xs 3,
                    LBytes.index xs 4,
                    LBytes.index xs 5,
                    LBytes.index xs 6,
                    LBytes.index xs 7
                )

    {-# INLINE word16Be #-}
    word16Be :: Parser LBytes.ByteString InputError Word16
    word16Be = mkParserWord16Be h
        where
            h :: LBytes.ByteString -> (Word8, Word8)
            h xs = (LBytes.index xs 1, LBytes.index xs 0)

    {-# INLINE word32Be #-}
    word32Be :: Parser LBytes.ByteString InputError Word32
    word32Be = mkParserWord32Be h
        where
            h :: LBytes.ByteString -> (Word8, Word8, Word8, Word8)
            h xs = (LBytes.index xs 3, LBytes.index xs 2, LBytes.index xs 1, LBytes.index xs 0)

    {-# INLINE word64Be #-}
    word64Be :: Parser LBytes.ByteString InputError Word64
    word64Be = mkParserWord64Be h
        where
            h :: LBytes.ByteString -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
            h xs =
                (
                    LBytes.index xs 7,
                    LBytes.index xs 6,
                    LBytes.index xs 5,
                    LBytes.index xs 4,
                    LBytes.index xs 3,
                    LBytes.index xs 2,
                    LBytes.index xs 1,
                    LBytes.index xs 0
                )

instance Binary SBytes.ShortByteString SBytes.ShortByteString where
    {-# INLINE word16Le #-}
    word16Le :: Parser SBytes.ShortByteString InputError Word16
    word16Le = mkParserWord16Le h
        where
            h :: SBytes.ShortByteString -> (Word8, Word8)
            h xs = (SBytes.index xs 0, SBytes.index xs 1)

    {-# INLINE word32Le #-}
    word32Le :: Parser SBytes.ShortByteString InputError Word32
    word32Le = mkParserWord32Le h
        where
            h :: SBytes.ShortByteString -> (Word8, Word8, Word8, Word8)
            h xs = (SBytes.index xs 0, SBytes.index xs 1, SBytes.index xs 2, SBytes.index xs 3)

    {-# INLINE word64Le #-}
    word64Le :: Parser SBytes.ShortByteString InputError Word64
    word64Le = mkParserWord64Le h
        where
            h :: SBytes.ShortByteString -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
            h xs =
                (
                    SBytes.index xs 0,
                    SBytes.index xs 1,
                    SBytes.index xs 2,
                    SBytes.index xs 3,
                    SBytes.index xs 4,
                    SBytes.index xs 5,
                    SBytes.index xs 6,
                    SBytes.index xs 7
                )

    {-# INLINE word16Be #-}
    word16Be :: Parser SBytes.ShortByteString InputError Word16
    word16Be = mkParserWord16Be h
        where
            h :: SBytes.ShortByteString -> (Word8, Word8)
            h xs = (SBytes.index xs 1, SBytes.index xs 0)

    {-# INLINE word32Be #-}
    word32Be :: Parser SBytes.ShortByteString InputError Word32
    word32Be = mkParserWord32Be h
        where
            h :: SBytes.ShortByteString -> (Word8, Word8, Word8, Word8)
            h xs = (SBytes.index xs 3, SBytes.index xs 2, SBytes.index xs 1, SBytes.index xs 0)

    {-# INLINE word64Be #-}
    word64Be :: Parser SBytes.ShortByteString InputError Word64
    word64Be = mkParserWord64Be h
        where
            h :: SBytes.ShortByteString -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
            h xs =
                (
                    SBytes.index xs 7,
                    SBytes.index xs 6,
                    SBytes.index xs 5,
                    SBytes.index xs 4,
                    SBytes.index xs 3,
                    SBytes.index xs 2,
                    SBytes.index xs 1,
                    SBytes.index xs 0
                )
