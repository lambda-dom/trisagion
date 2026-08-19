{- |
Module: Trisagion.Typeclasses.Serializers.Binary

The @Binary@ typeclass for serializers with @'Sink' 'Word8' b s@ constraints.
-}

module Trisagion.Typeclasses.Serializers.Binary (
    -- * Typeclasses.
    Binary (..),
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (view)

-- non-Hackage libraries.
import Data.Int.Optics (int8ToWord8)

-- Package.
import Trisagion.Typeclasses.Sink (Sink, single)
import Trisagion.Serializer (Serializer, embed)


{- | The @Binary@ typeclass for efficient serializers for machine-width types. -}
class Sink Word8 b s => Binary b s where
    {- | Serialize a single 'Word8'. -}
    {-# INLINE word8 #-}
    word8 :: Serializer s Word8
    word8 = embed single

    {- | Serialize a single 'Int8'. -}
    {-# INLINE int8 #-}
    int8 :: Serializer s Int8
    int8 = contramap (view int8ToWord8) word8

