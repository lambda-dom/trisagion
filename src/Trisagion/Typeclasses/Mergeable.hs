{- |
Module: Trisagion.Typeclasses.Mergeable

The @Mergeable@ typeclass for builders of 'Splittable' input streams.
-}

module Trisagion.Typeclasses.Mergeable (
    -- * Typeclasses.
    Mergeable (..),
) where

-- Imports.
-- Libraries.
import Data.ByteString.Lazy as LazyBytes (ByteString)
import qualified Data.ByteString.Builder as Bytes (Builder, lazyByteString)
import Data.Text.Lazy as LazyText (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder, fromLazyText)

-- Package.
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Typeclasses.Builder (Builder (..))


{- | The builder typeclass for 'Splittable' streams.

We assume that all overriden instance implementations are extensionally equal to the default ones.
There are two laws, the 'Mergeable' typeclass should satisfy:

__Compatibility__: 'merge' should be compatible with 'Builder.one' in the sense that:

prop> merge . single == one

__Monoid morphism__: with the constraint 'Monoid (PrefixOf (BuilderOf m))', 'merge' is a monoid
morphism.
-}
class (Builder m, Splittable (BuilderOf m)) => Mergeable m where
    {-# MINIMAL merge #-}

    {- | Build from a @'PrefixOf' ('BuilderOf' m)@. -}
    merge :: PrefixOf (BuilderOf m) -> m


-- Instances.
instance Mergeable Bytes.Builder where
    merge :: ByteString -> Bytes.Builder
    merge = Bytes.lazyByteString

instance Mergeable Text.Builder where
    merge :: Text -> Text.Builder
    merge = Text.fromLazyText
