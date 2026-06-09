{- |
Module: Trisagion.Serializers.Binary

The @Binary@ typeclass for serializers with constraints @'Sink' Word8 b s@.
-}

module Trisagion.Serializers.Binary (
    -- * Typeclasses.
    Binary (..),

    -- * Generic serializers.
    integralLe,
    integralBe,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8)
import Data.Word (Word8)

-- Package.
import Trisagion.Typeclasses.Sink (Sink (..))
import Trisagion.Serializer (Serializer, embed)


{- | The @Binary@ typeclass for efficient serializers for machine-width types. -}
class Sink Word8 b s => Binary b s where

    {- | Serialize a single 'Word8'. -}
    word8 :: Serializer s Word8
    word8 = embed $ snoc mempty

    {- | Serialize a single 'Int8'. -}
    int8 :: Serializer s Int8
    int8 = contramap fromIntegral word8


{- | Serialize machine-width integral in little-endian format. -}
{-# INLINEABLE integralLe #-}
integralLe :: Serializer s a
integralLe = embed $ undefined

{- | Serialize machine-width integral in big-endian format. -}
{-# INLINEABLE integralBe #-}
integralBe :: Serializer s a
integralBe = embed $ undefined
