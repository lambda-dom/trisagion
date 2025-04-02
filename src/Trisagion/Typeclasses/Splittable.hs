{- |
Module: Trisagion.Typeclasses.Splittable

The @Splittable@ typeclass.
-}

module Trisagion.Typeclasses.Splittable (
    -- * Typeclasses.
    Splittable (..),
) where

-- Imports.
-- Base.
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, span, splitAt)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, span, splitAt)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, span, splitAt)
import qualified Data.Text as Text (Text, span, splitAt)
import qualified Data.Text.Lazy as LazyText (Text, span, splitAt)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | The @Splittable@ typeclass of monomorphic splittable functors.

Mirroring the laws for the 'Streamable' typeclass, the first law is:

__Mononaturality__: With the constraints @('MonoFunctor' ('PrefixOf' s), 'ElementOf' ('PrefixOf' s)
~ 'ElementOf' s)@, for every @n@ and every @p@, both @splitAt n@ and @splitWith p@ are mononatural.

For the second law, assuming @MonoFoldable ('PrefixOf' s)@ besides the @'MonoFunctor'@ constraint,
then at the level of lists @splitAt@ is 'Data.List.splitAt' and @splitWith@ is 'Data.List.span':

__List identities__:

prop> bimap monotoList toList . splitAt n = splitAt n . toList
prop> bimap monotoList toList . splitWith p = span p . toList

The third and final law is a compatibility condition between
'Trisagion.Typeclasses.Streamable.uncons' and @splitAt@:

__Compatibility__:

prop> maybe [] (fmap (bimap singleton toList)) . uncons = bimap monotoList toList . splitAt 1
 -}
class Streamable s => Splittable s where
    {-# MINIMAL splitAt, splitWith #-}

    {- | The type of prefixes of the streamable. -}
    type PrefixOf s :: Type

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@. -}
    splitAt :: Word -> s -> (PrefixOf s, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.

    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    splitWith :: (ElementOf s -> Bool) -> s -> (PrefixOf s, s)

-- Instances.
instance Splittable Bytes.ByteString where
    type PrefixOf Bytes.ByteString = Bytes.ByteString

    splitAt :: Word -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitAt n = Bytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitWith = Bytes.span

instance Splittable LazyBytes.ByteString where
    type PrefixOf LazyBytes.ByteString = LazyBytes.ByteString

    splitAt :: Word -> LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    splitAt n = LazyBytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    splitWith = LazyBytes.span

instance Splittable ShortBytes.ShortByteString where
    type PrefixOf ShortBytes.ShortByteString = ShortBytes.ShortByteString

    splitAt :: Word -> ShortBytes.ShortByteString -> (ShortBytes.ShortByteString, ShortBytes.ShortByteString)
    splitAt n = ShortBytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> ShortBytes.ShortByteString -> (ShortBytes.ShortByteString, ShortBytes.ShortByteString)
    splitWith = ShortBytes.span

instance Splittable Text.Text where
    type PrefixOf Text.Text = Text.Text

    splitAt :: Word -> Text.Text -> (Text.Text, Text.Text)
    splitAt n = Text.splitAt $ fromIntegral n

    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

instance Splittable LazyText.Text where
    type PrefixOf LazyText.Text = LazyText.Text

    splitAt :: Word -> LazyText.Text -> (LazyText.Text, LazyText.Text)
    splitAt n = LazyText.splitAt $ fromIntegral n

    splitWith :: (Char -> Bool) -> LazyText.Text -> (LazyText.Text, LazyText.Text)
    splitWith = LazyText.span
