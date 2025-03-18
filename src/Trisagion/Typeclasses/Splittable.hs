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
import qualified Data.ByteString as Bytes (ByteString, span, splitAt)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, span, splitAt)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, span, splitAt)
import Data.Sequence (Seq)
import qualified Data.Text as Text (Text, span, splitAt)
import qualified Data.Text.Lazy as LazyText (Text, span, splitAt)
import qualified Data.Sequence as Seq (spanl, splitAt)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (span, splitAt)
import qualified Data.Vector.Strict as StrictVector (Vector, span, splitAt)
import qualified Data.Vector.Unboxed as UnboxedVector (Vector, Unbox, span, splitAt)
import qualified Data.Vector.Storable as StorableVector (Vector, Storable, span, splitAt)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))


{- | The @Splittable@ typeclass of monomorphic splittable functors.

Mirroring the laws for the 'Streamable' typeclass, the first law is:

__Naturality__: With @('MonoFunctor' ('PrefixOf' s), 'ElementOf' ('PrefixOf' s) ~ 'ElementOf' s)@,
for every @n@ and every @p@, both @splitAt n@ and @splitWith p@ are mononatural.

For the second law, assuming @MonoFoldable ('PrefixOf' s)@ besides the above @'MonoFunctor'@
constraint, then at the level of lists @splitAt@ is 'Data.List.splitAt' and @splitWith@ is
'Data.List.span':

__List identities__:

prop> bimap monotoList monotoList . splitAt n = splitAt n . monotoList
prop> bimap monotoList monotoList . splitWith p = span p . monotoList

The third and final law is a compatibility condition between 'splitOne' and @splitAt@:

__Compatibility__:

prop> maybe [] (bimap singleton monotoList) . splitOne = bimap monotoList monotoList . splitAt 1
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

instance Splittable [a] where
    type PrefixOf [a] = [a]

    splitAt :: Word -> [a] -> ([a], [a])
    splitAt n = List.splitAt $ fromIntegral n

    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith = span

instance Splittable (Seq a) where
    type PrefixOf (Seq a) = Seq a

    splitAt :: Word -> Seq a -> (Seq a, Seq a)
    splitAt n = Seq.splitAt $ fromIntegral n

    splitWith :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    splitWith = Seq.spanl

instance Splittable (Vector a) where
    type PrefixOf (Vector a) = Vector a

    splitAt :: Word -> Vector a -> (Vector a, Vector a)
    splitAt n = Vector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
    splitWith = Vector.span

instance Splittable (StrictVector.Vector a) where
    type PrefixOf (StrictVector.Vector a) = StrictVector.Vector a

    splitAt :: Word -> StrictVector.Vector a -> (StrictVector.Vector a, StrictVector.Vector a)
    splitAt n = StrictVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> StrictVector.Vector a -> (StrictVector.Vector a, StrictVector.Vector a)
    splitWith = StrictVector.span

instance UnboxedVector.Unbox a => Splittable (UnboxedVector.Vector a) where
    type PrefixOf (UnboxedVector.Vector a) = UnboxedVector.Vector a

    splitAt :: Word -> UnboxedVector.Vector a -> (UnboxedVector.Vector a, UnboxedVector.Vector a)
    splitAt n = UnboxedVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> UnboxedVector.Vector a -> (UnboxedVector.Vector a, UnboxedVector.Vector a)
    splitWith = UnboxedVector.span

instance StorableVector.Storable a => Splittable (StorableVector.Vector a) where
    type PrefixOf (StorableVector.Vector a) = StorableVector.Vector a

    splitAt :: Word -> StorableVector.Vector a -> (StorableVector.Vector a, StorableVector.Vector a)
    splitAt n = StorableVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> StorableVector.Vector a -> (StorableVector.Vector a, StorableVector.Vector a)
    splitWith = StorableVector.span
