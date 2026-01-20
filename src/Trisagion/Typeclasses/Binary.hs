{- |
Module: Trisagion.Typeclasses.Binary

The @Binary@ for parsers with constraints @'Streamable' m Word8 s@.
-}

module Trisagion.Typeclasses.Binary (
    -- * Generic parsers.
    integralLe,
    integralBe,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Word (Word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Utils.Integral (byteCount, pack, packReverse)
import Trisagion.ParserT (ParserT)
import Trisagion.Typeclasses.Streamable (InputError)
import Trisagion.Typeclasses.Splittable (Splittable (..))


{- | Parse a machine-width integral in little-endian format. -}
{-# INLINEABLE integralLe #-}
integralLe
    :: forall m b s w
    .  (Splittable m Word8 b s, MonoFoldable Word8 b, Integral w, FiniteBits w)
    => ParserT s InputError m w
integralLe = do
        bs <- takeExact $ byteCount @w 0
        pure $ pack (monotoList bs)

{- | Parse a machine-width integral in big-endian format. -}
{-# INLINEABLE integralBe #-}
integralBe
    :: forall m b s w
    .  (Splittable m Word8 b s, MonoFoldable Word8 b, Integral w, FiniteBits w)
    => ParserT s InputError m w
integralBe = do
        s <- takeExact $ byteCount @w 0
        pure $ packReverse (monotoList s)
