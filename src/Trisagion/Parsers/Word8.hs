{- |
Module: Trisagion.Parsers.Word8

Parsers @('Streamable' s, 'ElementOf' s ~ Word8) => 'ParserPE' s@.
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
import Data.Bits (Bits(..), FiniteBits (..))
import Data.Foldable (Foldable (..))
import Data.Int (Int8)
import Data.Word (Word8, Word16, Word32, Word64)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition)
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser (InputError, ParserPE, one)
import Trisagion.Parsers.Splittable (takeExact)


{- | Parse a single 'Word8'. -}
word8 :: (HasPosition s, Streamable s, ElementOf s ~ Word8) => ParserPE s InputError Word8
word8 = one

{- | Parse a single 'Int8'. -}
int8 :: (HasPosition s, Streamable s, ElementOf s ~ Word8) => ParserPE s InputError Int8
int8 = fromIntegral <$> one


{- | Parse a machine-width integral in little-endian format. -}
integralLe
    :: forall s w
    .  (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8, Integral w, FiniteBits w)
    => ParserPE s InputError w
integralLe = do
        s <- takeExact $ fromIntegral n
        let xs = zip [0 .. n - 1] (monotoList s)
        pure $ foldl' (.|.) 0 [shiftL (fromIntegral b) (i * 8) | (i, b) <- xs]
    where
        n :: Int
        n = finiteBitSize @w 0 `quot` 8

{- | Parse a machine-width integral in big-endian format. -}
integralBe
    :: forall s w
    .  (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8, Integral w, FiniteBits w)
    => ParserPE s InputError w
integralBe = do
        s <- takeExact $ fromIntegral n
        let xs = zip [n - 1, n - 2 .. 0] (monotoList s)
        pure $ foldl' (.|.) 0 [shiftL (fromIntegral b) (i * 8) | (i, b) <- xs]
    where
        n :: Int
        n = finiteBitSize @w 0 `quot` 8


{- | Parse a 'Word16' in little-endian format. -}
word16Le
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => ParserPE s InputError Word16
word16Le = integralLe

{- | Parse a 'Word32' in little-endian format. -}
word32Le
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => ParserPE s InputError Word32
word32Le = integralLe

{- | Parse a 'Word64' in little-endian format. -}
word64Le
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => ParserPE s InputError Word64
word64Le = integralLe

{- | Parse a 'Word16' in big-endian format. -}
word16Be
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => ParserPE s InputError Word16
word16Be = integralBe

{- | Parse a 'Word32' in big-endian format. -}
word32Be
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => ParserPE s InputError Word32
word32Be = integralBe

{- | Parse a 'Word64' in big-endian format. -}
word64Be
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => ParserPE s InputError Word64
word64Be = integralBe
