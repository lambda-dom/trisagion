{-# LANGUAGE AllowAmbiguousTypes #-}

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
import qualified Data.ByteString as Bytes (ByteString, span, splitAt, empty, drop, dropWhile, singleton)
import qualified Data.ByteString.Lazy as LBytes (ByteString, span, splitAt, empty, drop, dropWhile, singleton)
import qualified Data.ByteString.Short as SBytes (ShortByteString, span, splitAt, empty, drop, dropWhile, singleton)
import qualified Data.Text as Text (Text, span, splitAt, empty, drop, dropWhile, singleton)
import qualified Data.Text.Lazy as LText (Text, span, splitAt, empty, drop, dropWhile, singleton)
import qualified Data.Sequence as Seq (Seq, spanl, splitAt, empty, drop, dropWhileL, singleton)
import qualified Data.Vector as Vector (Vector, span, splitAt, empty, drop, dropWhile, singleton)
import qualified Data.Vector.Strict as SVector (Vector, span, splitAt, empty, drop, dropWhile, singleton)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, span, splitAt, empty, drop, dropWhile, singleton)
import qualified Data.Vector.Storable as StVector (Vector, Storable, span, splitAt, empty, drop, dropWhile, singleton)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)


{- | The @Splittable@ typeclass of monomorphic splittable functors.

Mirroring the laws for the 'Streamable' typeclass, the first law is:

__Mononaturality__: With the constraints @('MonoFunctor' ('PrefixOf' s), 'ElementOf' ('PrefixOf' s)
~ 'ElementOf' s)@, for every @n@ and every @p@, @splitPrefix n@, @splitWith p@ and @single@ are
mononatural.

__Single singleton__: The second law says that @single@ is @singleton@ at the level of lists:

prop> singleton == toList . single

__List identities__: For the third law, assuming
@'Mono.Typeclasses.MonoFoldable.MonoFoldable' ('PrefixOf' s)@ on top of the @'MonoFunctor'@
constraint, then at the level of lists @splitPrefix@ is 'Data.List.splitAt' and @splitWith@ is
'Data.List.span':

prop> bimap monotoList toList . splitPrefix n == splitPrefix n . toList
prop> bimap monotoList toList . splitWith p == span p . toList

__Compatibility__: The fourth and final law is a compatibility condition between
'Trisagion.Typeclasses.Streamable.uncons' and @splitPrefix@:

prop> maybe ([], []) (bimap singleton toList) . uncons == bimap monotoList toList . splitPrefix 1
 -}
class Streamable s => Splittable s where
    {-# MINIMAL splitPrefix, splitWith, single #-}

    {- | The type of prefixes of the streamable. -}
    type PrefixOf s :: Type

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@.

    The @prefix@ has size @n@, or less in case there is not enough input in the stream. In this
    case, the @suffix@ remainder is null.
    -}
    splitPrefix :: Word -> s -> (PrefixOf s, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.

    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    splitWith :: (ElementOf s -> Bool) -> s -> (PrefixOf s, s)

    {- | Convert an @'ElementOf' s@ to a prefix @'PrefixOf' s@.

    note(s):
    
      * Because of lack of injectivity, this method requires the @AllowAmbiguousTypes@ extension to
      keep GHC happy and inference commonly requires help from the user, e. g. instead of
      @'single'@, use @'single' \@s@ with the @TypeApplications@ extension.
    -}
    single :: ElementOf s -> PrefixOf s

    {- | Return the remainder of the stream as a prefix.

    Default implementation is:

    @splitRemainder = splitWith (const True)@

    Instances can (almost) always define a faster implementation using a nullary operation
    @empty :: s@.
    -}
    splitRemainder :: s -> (PrefixOf s, s)
    splitRemainder = splitWith (const True)

    {- | Drop a fixed-size prefix from the stream.

    Default implementation is:

    @dropPrefix n = snd . splitPrefix n@.
    -}
    dropPrefix :: Word -> s -> s
    dropPrefix n = snd . splitPrefix n

    {- | Drop the longest prefix satisfying a predicate from the stream.

    Default implementation is:

    @dropWith = snd . splitWith p@.
    -}
    dropWith :: (ElementOf s -> Bool) -> s -> s
    dropWith p = snd . splitWith p

-- Instances.
instance Splittable Bytes.ByteString where
    type PrefixOf Bytes.ByteString = Bytes.ByteString

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitPrefix n = Bytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitWith = Bytes.span

    {-# INLINE single #-}
    single :: Word8 -> Bytes.ByteString
    single = Bytes.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitRemainder s = (s, Bytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> Bytes.ByteString -> Bytes.ByteString
    dropPrefix n = Bytes.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> Bytes.ByteString -> Bytes.ByteString
    dropWith = Bytes.dropWhile

instance Splittable LBytes.ByteString where
    type PrefixOf LBytes.ByteString = LBytes.ByteString

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitPrefix n = LBytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitWith = LBytes.span

    {-# INLINE single #-}
    single :: Word8 -> LBytes.ByteString
    single = LBytes.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitRemainder s = (s, LBytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> LBytes.ByteString -> LBytes.ByteString
    dropPrefix n = LBytes.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> LBytes.ByteString -> LBytes.ByteString
    dropWith = LBytes.dropWhile

instance Splittable SBytes.ShortByteString where
    type PrefixOf SBytes.ShortByteString = SBytes.ShortByteString

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitPrefix n = SBytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitWith = SBytes.span

    {-# INLINE single #-}
    single :: Word8 -> SBytes.ShortByteString
    single = SBytes.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitRemainder s = (s, SBytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> SBytes.ShortByteString -> SBytes.ShortByteString
    dropPrefix n = SBytes.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> SBytes.ShortByteString
    dropWith = SBytes.dropWhile

instance Splittable Text.Text where
    type PrefixOf Text.Text = Text.Text

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Text.Text -> (Text.Text, Text.Text)
    splitPrefix n = Text.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

    {-# INLINE single #-}
    single :: Char -> Text.Text
    single = Text.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: Text.Text -> (Text.Text, Text.Text)
    splitRemainder s = (s, Text.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> Text.Text -> Text.Text
    dropPrefix n = Text.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> Text.Text -> Text.Text
    dropWith = Text.dropWhile

instance Splittable LText.Text where
    type PrefixOf LText.Text = LText.Text

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> LText.Text -> (LText.Text, LText.Text)
    splitPrefix n = LText.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> LText.Text -> (LText.Text, LText.Text)
    splitWith = LText.span

    {-# INLINE single #-}
    single :: Char -> LText.Text
    single = LText.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: LText.Text -> (LText.Text, LText.Text)
    splitRemainder s = (s, LText.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> LText.Text -> LText.Text
    dropPrefix n = LText.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> LText.Text -> LText.Text
    dropWith = LText.dropWhile

instance Splittable [a] where
    type PrefixOf [a] = [a]

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> [a] -> ([a], [a])
    splitPrefix n = List.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith = span

    {-# INLINE single #-}
    single :: a -> [a]
    single x = [x]

    {-# INLINE splitRemainder #-}
    splitRemainder :: [a] -> ([a], [a])
    splitRemainder xs = (xs, [])

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> [a] -> [a]
    dropPrefix n = drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> [a] -> [a]
    dropWith = dropWhile

instance Splittable (Seq.Seq a) where
    type PrefixOf (Seq.Seq a) = Seq.Seq a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    splitPrefix n = Seq.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    splitWith = Seq.spanl

    {-# INLINE single #-}
    single :: a -> Seq.Seq a
    single = Seq.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
    splitRemainder xs = (xs, Seq.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> Seq.Seq a -> Seq.Seq a
    dropPrefix n = Seq.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> Seq.Seq a -> Seq.Seq a
    dropWith = Seq.dropWhileL

instance Splittable (Vector.Vector a) where
    type PrefixOf (Vector.Vector a) = Vector.Vector a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitPrefix n = Vector.splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitWith = Vector.span

    {-# INLINE single #-}
    single :: a -> Vector.Vector a
    single = Vector.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitRemainder xs = (xs, Vector.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> Vector.Vector a -> Vector.Vector a
    dropPrefix n = Vector.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> Vector.Vector a -> Vector.Vector a
    dropWith = Vector.dropWhile

instance Splittable (SVector.Vector a) where
    type PrefixOf (SVector.Vector a) = SVector.Vector a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitPrefix n = SVector.splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitWith = SVector.span

    {-# INLINE single #-}
    single :: a -> SVector.Vector a
    single = SVector.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitRemainder xs = (xs, SVector.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> SVector.Vector a -> SVector.Vector a
    dropPrefix n = SVector.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> SVector.Vector a -> SVector.Vector a
    dropWith = SVector.dropWhile

instance UVector.Unbox a => Splittable (UVector.Vector a) where
    type PrefixOf (UVector.Vector a) = UVector.Vector a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitPrefix n = UVector.splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitWith = UVector.span

    {-# INLINE single #-}
    single :: a -> UVector.Vector a
    single = UVector.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitRemainder xs = (xs, UVector.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> UVector.Vector a -> UVector.Vector a
    dropPrefix n = UVector.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> UVector.Vector a -> UVector.Vector a
    dropWith = UVector.dropWhile

instance StVector.Storable a => Splittable (StVector.Vector a) where
    type PrefixOf (StVector.Vector a) = StVector.Vector a

    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitPrefix n = StVector.splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitWith = StVector.span

    {-# INLINE single #-}
    single :: a -> StVector.Vector a
    single = StVector.singleton

    {-# INLINE splitRemainder #-}
    splitRemainder :: StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitRemainder xs = (xs, StVector.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> StVector.Vector a -> StVector.Vector a
    dropPrefix n = StVector.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> StVector.Vector a -> StVector.Vector a
    dropWith = StVector.dropWhile
