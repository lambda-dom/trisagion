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
import qualified Data.ByteString.Lazy as LBytes (ByteString, span, splitAt, empty)
import qualified Data.ByteString.Short as SBytes (ShortByteString, span, splitAt, empty)
import qualified Data.Text as Text (Text, span, splitAt, empty)
import qualified Data.Text.Lazy as LText (Text, span, splitAt, empty)
import qualified Data.Sequence as Seq (Seq, spanl, splitAt, empty)
import qualified Data.Vector as Vector (Vector, span, splitAt, empty)
import qualified Data.Vector.Strict as SVector (Vector, span, splitAt, empty)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, span, splitAt, empty)
import qualified Data.Vector.Storable as StVector (Vector, Storable, span, splitAt, empty)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

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

    Default implementation is @'splitWith' (const True)@ but instances almost always can define a
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

instance Splittable LBytes.ByteString where
    type PrefixOf LBytes.ByteString = LBytes.ByteString

    splitAt :: Word -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitAt n = LBytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitWith = LBytes.span

    remainder :: LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    remainder s = (s, LBytes.empty)

instance Splittable SBytes.ShortByteString where
    type PrefixOf SBytes.ShortByteString = SBytes.ShortByteString

    splitAt :: Word -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitAt n = SBytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitWith = SBytes.span

    remainder :: SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    remainder s = (s, SBytes.empty)

instance Splittable Text.Text where
    type PrefixOf Text.Text = Text.Text

    splitAt :: Word -> Text.Text -> (Text.Text, Text.Text)
    splitAt n = Text.splitAt $ fromIntegral n

    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

    remainder :: Text.Text -> (Text.Text, Text.Text)
    remainder s = (s, Text.empty)

instance Splittable LText.Text where
    type PrefixOf LText.Text = LText.Text

    splitAt :: Word -> LText.Text -> (LText.Text, LText.Text)
    splitAt n = LText.splitAt $ fromIntegral n

    splitWith :: (Char -> Bool) -> LText.Text -> (LText.Text, LText.Text)
    splitWith = LText.span

    remainder :: LText.Text -> (LText.Text, LText.Text)
    remainder s = (s, LText.empty)

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

instance Splittable (SVector.Vector a) where
    type PrefixOf (SVector.Vector a) = SVector.Vector a

    splitAt :: Word -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitAt n = SVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitWith = SVector.span

    remainder :: SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    remainder xs = (xs, SVector.empty)

instance UVector.Unbox a => Splittable (UVector.Vector a) where
    type PrefixOf (UVector.Vector a) = UVector.Vector a

    splitAt :: Word -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitAt n = UVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitWith = UVector.span

    remainder :: UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    remainder xs = (xs, UVector.empty)

instance StVector.Storable a => Splittable (StVector.Vector a) where
    type PrefixOf (StVector.Vector a) = StVector.Vector a

    splitAt :: Word -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitAt n = StVector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitWith = StVector.span

    remainder :: StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    remainder xs = (xs, StVector.empty)
