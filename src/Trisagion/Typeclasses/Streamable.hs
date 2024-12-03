{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import qualified Data.List as List (uncons)

-- Libraries.
import Data.MonoTraversable (MonoFunctor (..), Element, MonoFoldable)
import Data.Sequence (Seq (..))
import Data.Vector (Vector)
import qualified Data.Text as Text (Text, uncons)
import qualified Data.Text.Lazy as LazyText (Text, uncons)
import qualified Data.ByteString as Bytes (ByteString, uncons)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, uncons)
import qualified Data.Vector as Vector (uncons)

-- Packages.
import qualified Trisagion.Lib.NonEmpty as NonEmpty (uncons)


{- | The @Streamable@ typeclass of monomorphic, streamable functors. -}
class (MonoFunctor s, MonoFoldable s) => Streamable s where
    {-# MINIMAL getOne #-}

    {- | Get, or uncons, the first element of the streamable. -}
    getOne :: s -> Maybe (Element s, s)


-- Instances.
instance Streamable Bytes.ByteString where
    getOne :: Bytes.ByteString -> Maybe (Word8, Bytes.ByteString)
    getOne = Bytes.uncons

instance Streamable LazyBytes.ByteString where
    getOne :: LazyBytes.ByteString -> Maybe (Word8, LazyBytes.ByteString)
    getOne = LazyBytes.uncons

instance Streamable Text.Text where
    getOne :: Text.Text -> Maybe (Char, Text.Text)
    getOne = Text.uncons

instance Streamable LazyText.Text where
    getOne :: LazyText.Text -> Maybe (Char, LazyText.Text)
    getOne = LazyText.uncons

instance Streamable [a] where
    getOne :: [a] -> Maybe (a, [a])
    getOne = List.uncons

instance Streamable (NonEmpty a) where
    getOne :: NonEmpty a -> Maybe (a, NonEmpty a)
    getOne = NonEmpty.uncons

instance Streamable (Seq a) where
    getOne :: Seq a -> Maybe (Element (Seq a), Seq a)
    getOne Empty      = Nothing
    getOne (x :<| xs) = Just (x, xs)

instance Streamable (Vector a) where
    getOne :: Vector a -> Maybe (a, Vector a)
    getOne = Vector.uncons
