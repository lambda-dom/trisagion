{- |
Module: Trisagion.Parsers.Binary

The @Binary@ for parsers with constraints @'Split' m Word8 b s@.
-}
{-# LANGUAGE UndecidableInstances #-}

module Trisagion.Parsers.Binary (
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
import Trisagion.Utils.Bits (bytecount, pack, packReverse)
import Trisagion.Typeclasses.Split (Split (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError, one)
import Trisagion.Parsers.Split (takeExact)


-- $setup
-- >>> import Trisagion.Parser


{- | The @Binary@ typeclass for efficient parsers for machine-width types. -}
class (Split Word8 b s, MonoFoldable Word8 b) => Binary b s where
    {- | Parse a single 'Word8'.

    === __Examples:__

    >>> parse word8 [0xff, 0, 0, 0]
    Right (255,[0,0,0])

    >>> parse word8 [1, 0, 0, 0]
    Right (1,[0,0,0])

    >>> parse word8 []
    Left (InputError 1)
    -}
    word8 :: Parser s InputError Word8
    word8 = one

    {- | Parse a single 'Int8'.

    === __Examples:__

    >>> parse int8 [1, 0, 0, 0]
    Right (1,[0,0,0])

    >>> parse int8 [fromIntegral (-1 :: Int), 0, 0, 0]
    Right (-1,[0,0,0])
    -}
    int8 :: Parser s InputError Int8
    int8 = fromIntegral <$> word8

    {- | Parse a 'Word16' in little-endian format. -}
    word16Le :: Parser s InputError Word16
    word16Le = integralLe

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
    word32Le :: Parser s InputError Word32
    word32Le = integralLe

    {- | Parse a 'Word64' in little-endian format. -}
    word64Le :: Parser s InputError Word64
    word64Le = integralLe

    {- | Parse a 'Word16' in big-endian format. -}
    word16Be :: Parser s InputError Word16
    word16Be = integralBe

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
    word32Be :: Parser s InputError Word32
    word32Be = integralBe

    {- | Parse a 'Word64' in big-endian format. -}
    word64Be :: Parser s InputError Word64
    word64Be = integralBe


-- Instances.
instance Binary [Word8] [Word8]


{- | Parse a machine-width integral in little-endian format. -}
{-# INLINEABLE integralLe #-}
integralLe
    :: forall b s a
    .  (Split Word8 b s, MonoFoldable Word8 b, Integral a, FiniteBits a)
    => Parser s InputError a
integralLe = do
        bs <- takeExact $ bytecount a
        pure $ pack (monotoList bs)

{- | Parse a machine-width integral in big-endian format. -}
{-# INLINEABLE integralBe #-}
integralBe
    :: forall b s a
    .  (Split Word8 b s, MonoFoldable Word8 b, Integral a, FiniteBits a)
    => Parser s InputError a
integralBe = do
        s <- takeExact $ bytecount a
        pure $ packReverse (monotoList s)
