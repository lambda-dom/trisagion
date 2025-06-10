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
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (toLazyByteString, word8, lazyByteString)
import qualified Data.ByteString.Builder as Bytes (Builder)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText, singleton, fromString, fromLazyText)
import qualified Data.Text.Lazy.Builder as Text (Builder)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | Typeclass for builders of streamable sequences.

The 'Builder' must satisfy the following law:

__Monoid morphism__: the function 'unpack' is a monoid morphism.
-}
class (Monoid m, Streamable (BuilderOf m)) => Builder m where
    {-# MINIMAL unpack, pack, one #-}

    {- | The type of sequence that the builder builds. -}
    type BuilderOf m :: Type

    {- | The monoid morphism with what the builder builds. -}
    unpack :: m -> BuilderOf m

    {- | The inverse of 'unpack'. -}
    pack :: BuilderOf m -> m

    {- | Build from one @'ElementOf' ('BuilderOf' m)@. -}
    one :: ElementOf (BuilderOf m) -> m

    {- | Build from a foldable of many @'ElementOf' ('BuilderOf' m)@.

    Default implementation is:

    @many = foldMap one@
    -}
    many :: Foldable t => t (ElementOf (BuilderOf m)) -> m
    many = foldMap one


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

instance Builder Text.Builder where
    type BuilderOf Text.Builder = Text

    {-# INLINE unpack #-}
    unpack :: Text.Builder -> Text
    unpack = toLazyText

    {-# INLINE pack #-}
    pack :: Text -> Text.Builder
    pack = fromLazyText

    {-# INLINE one #-}
    one :: Char -> Text.Builder
    one = singleton

    {-# INLINE many #-}
    many :: Foldable t => t Char -> Text.Builder
    many = fromString . toList
