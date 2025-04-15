{- |
Module: Trisagion.Typeclasses.Mergeable

The @Mergeable@ typeclass for builders of 'Splittable'.
-}

module Trisagion.Typeclasses.Mergeable (
    -- * Typeclasses.
    Mergeable (..),
) where

-- Imports.
-- Libraries.
import qualified Data.ByteString.Lazy as LazyBytes (ByteString)
import qualified Data.ByteString.Builder as Bytes (Builder, lazyByteString)
import qualified Data.Text.Lazy as LazyText (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder, fromLazyText)

-- Package.
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Typeclasses.Builder (Builder (..))


{- | The builder typeclass for 'Splittable' streams. -}
class (Builder m, Splittable (BuilderOf m)) => Mergeable m where
    {-# MINIMAL prefix #-}

    {- | Build from a @'PrefixOf' ('BuilderOf' m)@. -}
    prefix :: PrefixOf (BuilderOf m) -> m


-- Instances.
instance Mergeable Bytes.Builder where
    prefix :: LazyBytes.ByteString -> Bytes.Builder
    prefix = Bytes.lazyByteString

instance Mergeable Text.Builder where
    prefix :: LazyText.Text -> Text.Builder
    prefix = Text.fromLazyText
