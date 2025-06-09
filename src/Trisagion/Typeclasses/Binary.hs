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
import Data.Word (Word8, Word16, Word32, Word64)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Types.ByteArray (bytes)

-- Package.
import Trisagion.Typeclasses.Builder (Builder (..))


{- | The @Binary@ typeclass for binary builders.

This class is used for optimization purposes only, as it does not add any new operations that
be done with the parent class.
-}
class (Builder m, ElementOf (BuilderOf m) ~ Word8) => Binary m where

    {- | Serialize an integral number in big-endian format. -}
    wordBe :: (Integral a, FiniteBits a) => a -> m

    {- | Specialization of 'wordBe' to 'Word16', -}
    word16Be :: Word16 -> m
    word16Be = wordLe

    {- | Specialization of 'wordBe' to 'Word32', -}
    word32Be :: Word32 -> m
    word32Be = wordBe

    {- | Specialization of 'wordBe' to 'Word64', -}
    word64Be :: Word64 -> m
    word64Be = wordBe

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


-- Instances.
