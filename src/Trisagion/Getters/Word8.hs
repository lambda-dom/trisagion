{- |
Module: Trisagion.Getters.Word8

Parsers @('Streamable' s, 'Element' s ~ Word8) => 'Get' s@.
-}

module Trisagion.Getters.Word8 (
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
import Data.Bits (FiniteBits (..), shiftL, (.|.))
import Data.Foldable (foldl')
import Data.Int (Int8)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Data.MonoTraversable (Element, MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Get (Get)
import Trisagion.Getters.Streamable (InputError (..), one)
import Trisagion.Getters.Splittable (takeExact)


{- | Get a single @'Word8'@ from the input. -}
word8 :: (Streamable s, Element s ~ Word8) => Get s (ParseError s InputError) Word8
word8 = one

{- | Get a single @'Int8'@ from the input. -}
int8 :: (Streamable s, Element s ~ Word8) => Get s (ParseError s InputError) Int8
int8 = fromIntegral <$> one

{- | Get a machine-width integral in little-endian format from the streamable. -}
integralLe
        :: forall s w
        .  (Splittable s, MonoFoldable (PrefixOf s), Element (PrefixOf s) ~ Word8, Integral w, FiniteBits w)
        => Get s (ParseError s InputError) w
integralLe = do
        s <- takeExact $ fromIntegral n
        let xs = zip [0 .. n - 1] (otoList s)
        pure $ foldl' (.|.) 0 [shiftL (fromIntegral b) (i * 8) | (i, b) <- xs]
    where
        n :: Int
        n = finiteBitSize @w 0 `quot` 8

{- | Get a machine-width integral in big-endian format from the streamable. -}
integralBe
        :: forall s w
        .  (Splittable s, MonoFoldable (PrefixOf s), Element (PrefixOf s) ~ Word8, Integral w, FiniteBits w)
        => Get s (ParseError s InputError) w
integralBe = do
        s <- takeExact $ fromIntegral n
        let xs = zip [n - 1, n - 2 .. 0] (otoList s)
        pure $ foldl' (.|.) 0 [shiftL (fromIntegral b) (i * 8) | (i, b) <- xs]
    where
        n :: Int
        n = finiteBitSize @w 0 `quot` 8

{- | Get a @'Word16'@ in little-endian format from the streamable. -}
word16Le
    :: (Splittable s, MonoFoldable (PrefixOf s), Element (PrefixOf s) ~ Word8)
    => Get s (ParseError s InputError) Word16
word16Le = integralLe

{- | Get a @'Word32'@ in little-endian format from the streamable. -}
word32Le
    :: (Splittable s, MonoFoldable (PrefixOf s), Element (PrefixOf s) ~ Word8)
    => Get s (ParseError s InputError) Word32
word32Le = integralLe

{- | Get a @'Word64'@ in little-endian format from the streamable. -}
word64Le
    :: (Splittable s, MonoFoldable (PrefixOf s), Element (PrefixOf s) ~ Word8)
    => Get s (ParseError s InputError) Word64
word64Le = integralLe

{- | Get a @'Word16'@ in big-endian format from the streamable. -}
word16Be
    :: (Splittable s, MonoFoldable (PrefixOf s), Element (PrefixOf s) ~ Word8)
    => Get s (ParseError s InputError) Word16
word16Be = integralBe

{- | Get a @'Word32'@ in big-endian format from the streamable. -}
word32Be
    :: (Splittable s, MonoFoldable (PrefixOf s), Element (PrefixOf s) ~ Word8)
    => Get s (ParseError s InputError) Word32
word32Be = integralBe

{- | Get a @'Word64'@ in big-endian format from the streamable. -}
word64Be
    :: (Splittable s, MonoFoldable (PrefixOf s), Element (PrefixOf s) ~ Word8)
    => Get s (ParseError s InputError) Word64
word64Be = integralBe
