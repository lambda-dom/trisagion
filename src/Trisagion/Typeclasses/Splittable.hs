{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use drop" #-}

{- |
Module: Trisagion.Typeclasses.Splittable

The @Splittable@ typeclass.
-}

module Trisagion.Typeclasses.Splittable (
    -- * Typeclasses.
    Splittable (..),

    -- * Functions.
    dropPrefix,
    dropWith,
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
    {-# MINIMAL splitPrefix , splitWith #-}

    {- | The type of prefixes of the streamable. -}
    type PrefixOf s :: Type

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@. -}
    splitPrefix :: Word -> s -> (PrefixOf s, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.

    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    splitWith :: (ElementOf s -> Bool) -> s -> (PrefixOf s, s)

    {- | Return the remainder of the stream as a prefix.

    Default implementation is @'splitWith' (const True)@ but instances can (almost) always define a
    faster implementation using a nullary operation @empty :: s@.
    -}
    splitRemainder :: s -> (PrefixOf s, s)
    splitRemainder = splitWith (const True)

-- Instances.
instance Splittable Bytes.ByteString where
    type PrefixOf Bytes.ByteString = Bytes.ByteString

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitPrefix n = Bytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitWith = Bytes.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitRemainder s = (s, Bytes.empty)

instance Splittable LBytes.ByteString where
    type PrefixOf LBytes.ByteString = LBytes.ByteString

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitPrefix n = LBytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitWith = LBytes.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitRemainder s = (s, LBytes.empty)

instance Splittable SBytes.ShortByteString where
    type PrefixOf SBytes.ShortByteString = SBytes.ShortByteString

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitPrefix n = SBytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitWith = SBytes.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitRemainder s = (s, SBytes.empty)

instance Splittable Text.Text where
    type PrefixOf Text.Text = Text.Text

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Text.Text -> (Text.Text, Text.Text)
    splitPrefix n = Text.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: Text.Text -> (Text.Text, Text.Text)
    splitRemainder s = (s, Text.empty)

instance Splittable LText.Text where
    type PrefixOf LText.Text = LText.Text

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> LText.Text -> (LText.Text, LText.Text)
    splitPrefix n = LText.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> LText.Text -> (LText.Text, LText.Text)
    splitWith = LText.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: LText.Text -> (LText.Text, LText.Text)
    splitRemainder s = (s, LText.empty)

instance Splittable [a] where
    type PrefixOf [a] = [a]

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> [a] -> ([a], [a])
    splitPrefix n = List.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith = span

    {-# INLINE splitRemainder #-}
    splitRemainder :: [a] -> ([a], [a])
    splitRemainder xs = (xs, [])

instance Splittable (Seq.Seq a) where
    type PrefixOf (Seq.Seq a) = Seq.Seq a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    splitPrefix n = Seq.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    splitWith = Seq.spanl

    {-# INLINE splitRemainder #-}
    splitRemainder :: Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    splitRemainder xs = (xs, Seq.empty)

instance Splittable (Vector.Vector a) where
    type PrefixOf (Vector.Vector a) = Vector.Vector a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitPrefix n = Vector.splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitWith = Vector.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitRemainder xs = (xs, Vector.empty)

instance Splittable (SVector.Vector a) where
    type PrefixOf (SVector.Vector a) = SVector.Vector a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitPrefix n = SVector.splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitWith = SVector.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitRemainder xs = (xs, SVector.empty)

instance UVector.Unbox a => Splittable (UVector.Vector a) where
    type PrefixOf (UVector.Vector a) = UVector.Vector a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitPrefix n = UVector.splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitWith = UVector.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitRemainder xs = (xs, UVector.empty)

instance StVector.Storable a => Splittable (StVector.Vector a) where
    type PrefixOf (StVector.Vector a) = StVector.Vector a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitPrefix n = StVector.splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitWith = StVector.span

    {-# INLINE splitRemainder #-}
    splitRemainder :: StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitRemainder xs = (xs, StVector.empty)


{- | Drop a fixed-size prefix from the stream. -}
{-# INLINE dropPrefix #-}
dropPrefix :: Splittable s => Word -> s -> s
dropPrefix n = snd . splitPrefix n

{- | Drop the longest prefix satisfying a predicate from the stream. -}
{-# INLINE dropWith #-}
dropWith :: Splittable s => (ElementOf s -> Bool) -> s -> s
dropWith p = snd . splitWith p
