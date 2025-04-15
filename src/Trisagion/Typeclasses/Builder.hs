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
import Data.Foldable (foldMap', toList)
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString.Lazy as LazyBytes (ByteString)
import qualified Data.ByteString.Builder as Bytes (Builder, toLazyByteString, word8)
import qualified Data.Text.Lazy as LazyText (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder, toLazyText, singleton, fromString)

-- non-Hackage libraries.
import Data.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | Typeclass for builders of streamable sequences. -}
class (Monoid m, Streamable (BuilderOf m)) => Builder m where
    {-# MINIMAL unpack, one #-}

    {- | The type of sequence that the builder builds. -}
    type BuilderOf m :: Type

    {- | The monoid-isomorphism with what the builder builds. -}
    unpack :: m -> BuilderOf m

    {- | Build from one @'ElementOf' ('BuilderOf' m)@. -}
    one :: ElementOf (BuilderOf m) -> m

    {- | Build from a foldable of many @'ElementOf' ('BuilderOf' m)@. -}
    many :: Foldable t => t (ElementOf (BuilderOf m)) -> m
    many = foldMap' one


-- Instances.
instance Builder Bytes.Builder where
    type BuilderOf Bytes.Builder = LazyBytes.ByteString

    unpack :: Bytes.Builder -> LazyBytes.ByteString
    unpack = Bytes.toLazyByteString

    one :: Word8 -> Bytes.Builder
    one = Bytes.word8

instance Builder Text.Builder where
    type BuilderOf Text.Builder = LazyText.Text

    unpack :: Text.Builder -> LazyText.Text
    unpack = Text.toLazyText

    one :: Char -> Text.Builder
    one = Text.singleton

    many :: Foldable t => t Char -> Text.Builder
    many = Text.fromString . toList
