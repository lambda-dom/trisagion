{- |
Module: Trisagion.Serializers.Binary

Serializers @'Binary' m => 'Serializer' m a@.
-}

module Trisagion.Serializers.Binary (
    -- * Serializers @'Binary' m => 'Serializer' m a@.
    word8,
    int8,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8)
import Data.Word (Word8)

-- Package.
import Trisagion.Typeclasses.Builder (one)
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Serializer (Serializer, embed)


{- | Serialize a 'Word8'. -}
word8 :: Binary m => Serializer m Word8
word8 = embed one

{- | Serialize an 'Int8'. -}
int8 :: Binary m => Serializer m Int8
int8 = contramap fromIntegral word8
