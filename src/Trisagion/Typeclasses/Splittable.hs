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
import qualified Data.List as List (splitAt)
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, span, splitAt, empty)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, span, splitAt, empty)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, span, splitAt, empty)
import qualified Data.Text as Text (Text, span, splitAt, empty)
import qualified Data.Text.Lazy as LazyText (Text, span, splitAt, empty)
import qualified Data.Sequence as Seq (Seq, spanl, splitAt, empty)
import qualified Data.Vector as Vector (Vector, span, splitAt, empty)
import qualified Data.Vector.Strict as StrictVector (Vector, span, splitAt, empty)
import qualified Data.Vector.Unboxed as UnboxedVector (Vector, Unbox, span, splitAt, empty)
import qualified Data.Vector.Storable as StorableVector (Vector, Storable, span, splitAt, empty)

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

    {- | Return the remainder of the stream.

    Default implementation is @'splitWit' (const True)@ but instances almost always can define a
    faster implementation using a nullary operation @empty :: s@.
    -}
    remainder :: s -> (PrefixOf s, s)
    remainder = splitWith (const True)

-- Instances.
instance Splittable Bytes.ByteString where
    type PrefixOf Bytes.ByteString = Bytes.ByteString

    splitAt :: Word -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitAt n = Bytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitWith = Bytes.span

    remainder :: Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    remainder s = (s, Bytes.empty)

instance Splittable LazyBytes.ByteString where
    type PrefixOf LazyBytes.ByteString = LazyBytes.ByteString

    splitAt :: Word -> LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    splitAt n = LazyBytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    splitWith = LazyBytes.span

    remainder :: LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    remainder s = (s, LazyBytes.empty)

instance Splittable ShortBytes.ShortByteString where
    type PrefixOf ShortBytes.ShortByteString = ShortBytes.ShortByteString

    splitAt :: Word -> ShortBytes.ShortByteString -> (ShortBytes.ShortByteString, ShortBytes.ShortByteString)
    splitAt n = ShortBytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> ShortBytes.ShortByteString -> (ShortBytes.ShortByteString, ShortBytes.ShortByteString)
    splitWith = ShortBytes.span

    remainder :: ShortBytes.ShortByteString -> (ShortBytes.ShortByteString, ShortBytes.ShortByteString)
    remainder s = (s, ShortBytes.empty)

instance Splittable Text.Text where
    type PrefixOf Text.Text = Text.Text

    splitAt :: Word -> Text.Text -> (Text.Text, Text.Text)
    splitAt n = Text.splitAt $ fromIntegral n

    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

    remainder :: Text.Text -> (Text.Text, Text.Text)
    remainder s = (s, Text.empty)

instance Splittable LazyText.Text where
    type PrefixOf LazyText.Text = LazyText.Text

    splitAt :: Word -> LazyText.Text -> (LazyText.Text, LazyText.Text)
    splitAt n = LazyText.splitAt $ fromIntegral n

    splitWith :: (Char -> Bool) -> LazyText.Text -> (LazyText.Text, LazyText.Text)
    splitWith = LazyText.span

    remainder :: LazyText.Text -> (LazyText.Text, LazyText.Text)
    remainder s = (s, LazyText.empty)

instance Splittable [a] where
    type PrefixOf [a] = [a]

    splitAt :: Word -> [a] -> ([a], [a])
    splitAt n = List.splitAt $ fromIntegral n

    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith = span

    remainder :: [a] -> ([a], [a])
    remainder xs = (xs, [])

instance Splittable (Seq.Seq a) where
    type PrefixOf (Seq.Seq a) = Seq.Seq a

    splitAt :: Word -> Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    splitAt n = Seq.splitAt $ fromIntegral n

    splitWith :: (a -> Bool) -> Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    splitWith = Seq.spanl

    remainder :: Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    remainder xs = (xs, Seq.empty)

instance Splittable (Vector.Vector a) where
    type PrefixOf (Vector.Vector a) = Vector.Vector a

    splitAt :: Word -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitAt n = Vector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitWith = Vector.span

    remainder :: Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    remainder xs = (xs, Vector.empty)

instance Splittable (StrictVector.Vector a) where
    type PrefixOf (StrictVector.Vector a) = StrictVector.Vector a

    splitAt :: Word -> StrictVector.Vector a -> (StrictVector.Vector a, StrictVector.Vector a)
    splitAt n = StrictVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> StrictVector.Vector a -> (StrictVector.Vector a, StrictVector.Vector a)
    splitWith = StrictVector.span

    remainder :: StrictVector.Vector a -> (StrictVector.Vector a, StrictVector.Vector a)
    remainder xs = (xs, StrictVector.empty)

instance UnboxedVector.Unbox a => Splittable (UnboxedVector.Vector a) where
    type PrefixOf (UnboxedVector.Vector a) = UnboxedVector.Vector a

    splitAt :: Word -> UnboxedVector.Vector a -> (UnboxedVector.Vector a, UnboxedVector.Vector a)
    splitAt n = UnboxedVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> UnboxedVector.Vector a -> (UnboxedVector.Vector a, UnboxedVector.Vector a)
    splitWith = UnboxedVector.span

    remainder :: UnboxedVector.Vector a -> (UnboxedVector.Vector a, UnboxedVector.Vector a)
    remainder xs = (xs, UnboxedVector.empty)

instance StorableVector.Storable a => Splittable (StorableVector.Vector a) where
    type PrefixOf (StorableVector.Vector a) = StorableVector.Vector a

    splitAt :: Word -> StorableVector.Vector a -> (StorableVector.Vector a, StorableVector.Vector a)
    splitAt n = StorableVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> StorableVector.Vector a -> (StorableVector.Vector a, StorableVector.Vector a)
    splitWith = StorableVector.span

    remainder :: StorableVector.Vector a -> (StorableVector.Vector a, StorableVector.Vector a)
    remainder xs = (xs, StorableVector.empty)
