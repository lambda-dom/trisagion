{- |
Module: Trisagion.Typeclasses.Builder

The @Builder@ typeclass for builders of streamables.
-}

module Trisagion.Typeclasses.Builder (
    -- * Typeclasses.
    Builder (..),
) where

-- Imports.
-- Base.
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (toLazyByteString, word8, lazyByteString)
import qualified Data.ByteString.Builder as Bytes (Builder)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | Typeclass for builders of streamable sequences.

The 'Builder' must satisfy the following two laws:

__Monoid isomorphism__: with the constraint @Monoid ('BuilderOf' m)@, the function 'unpack' is a
monoid morphism and has an inverse.

__Singleton__: compatibility between 'one' and 'unpack':

prop> toList . unpack . one = singleton
-}
class (Monoid a, Streamable (BuilderOf a)) => Builder a where
    {-# MINIMAL unpack, pack, one #-}

    {- | The type of sequence that the builder builds. -}
    type BuilderOf a :: Type

    {- | The monoid morphism with what the builder builds. -}
    unpack :: a -> BuilderOf a

    {- | The inverse of 'unpack'. -}
    pack :: BuilderOf a -> a

    {- | Build from one @'ElementOf' ('BuilderOf' m)@. -}
    one :: ElementOf (BuilderOf a) -> a


-- Instances.
instance Builder Bytes.Builder where
    type BuilderOf Bytes.Builder = ByteString

    {-# INLINE unpack #-}
    unpack :: Bytes.Builder -> ByteString
    unpack = toLazyByteString

    {-# INLINE pack #-}
    pack :: ByteString -> Bytes.Builder
    pack = lazyByteString

    {-# INLINE one #-}
    one :: Word8 -> Bytes.Builder
    one = word8
