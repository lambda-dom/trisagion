{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),

    -- * Functions.
    isSuffix,
) where

-- Imports.
-- Base.
import qualified Data.Foldable as Foldable (null, toList)
import Data.List (unfoldr, singleton)
import qualified Data.List as List (uncons, isSuffixOf)
import Data.Maybe (isNothing)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, uncons, null, unpack, drop)
import qualified Data.ByteString.Lazy as LBytes (ByteString, uncons, null, unpack, drop)
import qualified Data.ByteString.Short as SBytes (ShortByteString, uncons, null, unpack, drop)
import qualified Data.Text as Text (Text, uncons, null, unpack, drop)
import qualified Data.Text.Lazy as LText (Text, uncons, null, unpack, drop)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq (drop)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (uncons, drop)
import qualified Data.Vector.Strict as SVector (Vector, uncons, drop)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, uncons, null, toList, drop)
import qualified Data.Vector.Storable as StVector (Vector, Storable, uncons, null, toList, drop)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))


{- | The @Streamable@ typeclass of monomorphic, streamable functors.

To describe the laws for the 'Streamable' typeclass, we start with a definition.

__Definition__: Let @s@ and @t@ be two monofunctors with @a ~ 'ElementOf' s ~ 'ElementOf' t@. A
function @h :: s -> t@ is /mononatural/ if for every @f :: a -> a@ we have the equality

@
monomap f . h = h . monomap f
@

Since @s@ is not polymorphic we do not have free theorems to rely on, so naturality must be
explicitly required:

__Naturality__: The function @'uncons' :: s -> 'Maybe' ('ElementOf' s, s)@ is mononatural.

In case it is not clear, the 'MonoFunctor' instance for @'Maybe' ('ElementOf' s, s)@ is:

@
monomap :: ('ElementOf' s -> 'ElementOf' s) -> 'Maybe' ('ElementOf' s, s) -> 'Maybe' ('ElementOf' s, s)
monomap f = fmap (bimap f (monomap f))
@
 
Given @'uncons' :: s -> 'Maybe' ('ElementOf' s, s)@ we can define @'toList' ::s -> [ElementOf s]@
by @'Data.List.unfoldr' 'uncons'@.

__Foldability__:

prop> MonoFoldable s => monotoList = toList

Finally, the third law says that 'uncons' really is uncons-ing at the level of lists.

__Unconsing__:

prop> toList = maybe [] (\ (x, xs) -> x : toList xs) . uncons
-}
class MonoFunctor s => Streamable s where
    {-# MINIMAL uncons #-}

    {- | Uncons the first element from the streamable. -}
    uncons :: s -> Maybe (ElementOf s, s)

    {- | Return 'True' if there are no elements in the input stream. -}
    {-# INLINE null #-}
    null :: s -> Bool
    null = isNothing . uncons

    {- | Drop one element from the stream.

    Default implementation is @maybe s snd $ uncons s@.
    -}
    {-# INLINE dropOne #-}
    dropOne :: s -> s
    dropOne s = maybe s snd $ uncons s

    {- | Convert a 'Streamable' to a list.

    Default implementation is @unfoldr uncons@.
    -}
    {-# INLINE toList #-}
    toList :: s -> [ElementOf s]
    toList = unfoldr uncons

-- Instances.
instance Streamable Bytes.ByteString where
    {-# INLINE uncons #-}
    uncons :: Bytes.ByteString -> Maybe (Word8, Bytes.ByteString)
    uncons = Bytes.uncons

    {-# INLINE null #-}
    null :: Bytes.ByteString -> Bool
    null = Bytes.null

    {-# INLINE dropOne #-}
    dropOne :: Bytes.ByteString -> Bytes.ByteString
    dropOne = Bytes.drop 1

    {-# INLINE toList #-}
    toList :: Bytes.ByteString -> [Word8]
    toList = Bytes.unpack

instance Streamable LBytes.ByteString where
    {-# INLINE uncons #-}
    uncons :: LBytes.ByteString -> Maybe (Word8, LBytes.ByteString)
    uncons = LBytes.uncons

    {-# INLINE null #-}
    null :: LBytes.ByteString -> Bool
    null = LBytes.null

    {-# INLINE dropOne #-}
    dropOne :: LBytes.ByteString -> LBytes.ByteString
    dropOne = LBytes.drop 1

    {-# INLINE toList #-}
    toList :: LBytes.ByteString -> [Word8]
    toList = LBytes.unpack

instance Streamable SBytes.ShortByteString where
    {-# INLINE uncons #-}
    uncons :: SBytes.ShortByteString -> Maybe (Word8, SBytes.ShortByteString)
    uncons = SBytes.uncons

    {-# INLINE null #-}
    null :: SBytes.ShortByteString -> Bool
    null = SBytes.null

    {-# INLINE dropOne #-}
    dropOne :: SBytes.ShortByteString -> SBytes.ShortByteString
    dropOne = SBytes.drop 1

    {-# INLINE toList #-}
    toList :: SBytes.ShortByteString -> [Word8]
    toList = SBytes.unpack

instance Streamable Text.Text where
    {-# INLINE uncons #-}
    uncons :: Text.Text -> Maybe (Char, Text.Text)
    uncons = Text.uncons

    {-# INLINE null #-}
    null :: Text.Text -> Bool
    null = Text.null

    {-# INLINE dropOne #-}
    dropOne :: Text.Text -> Text.Text
    dropOne = Text.drop 1

    {-# INLINE toList #-}
    toList :: Text.Text -> [Char]
    toList = Text.unpack

instance Streamable LText.Text where
    {-# INLINE uncons #-}
    uncons :: LText.Text -> Maybe (Char, LText.Text)
    uncons = LText.uncons

    {-# INLINE null #-}
    null :: LText.Text -> Bool
    null = LText.null

    {-# INLINE dropOne #-}
    dropOne :: LText.Text -> LText.Text
    dropOne = LText.drop 1

    {-# INLINE toList #-}
    toList :: LText.Text -> [Char]
    toList = LText.unpack

instance Streamable (Maybe a) where
    {-# INLINE uncons #-}
    uncons :: Maybe a -> Maybe (a, Maybe a)
    uncons Nothing = Nothing
    uncons (Just x) = Just (x, Nothing)

    {-# INLINE null #-}
    null :: Maybe a -> Bool
    null = isNothing

    {-# INLINE dropOne #-}
    dropOne :: Maybe a -> Maybe a
    dropOne = const Nothing

    {-# INLINE toList #-}
    toList :: Maybe a -> [a]
    toList = maybe [] singleton

instance Streamable [a] where
    {-# INLINE uncons #-}
    uncons :: [a] -> Maybe (a, [a])
    uncons = List.uncons

    {-# INLINE null #-}
    null :: [a] -> Bool
    null = Foldable.null

    {-# INLINE dropOne #-}
    dropOne :: [a] -> [a]
    dropOne = drop 1

    {-# INLINE toList #-}
    toList :: [a] -> [a]
    toList = id

instance Streamable (Seq a) where
    {-# INLINE uncons #-}
    uncons :: Seq a -> Maybe (a, Seq a)
    uncons Empty      = Nothing
    uncons (x :<| xs) = Just (x, xs)

    {-# INLINE null #-}
    null :: Seq a -> Bool
    null = Foldable.null

    {-# INLINE dropOne #-}
    dropOne :: Seq a -> Seq a
    dropOne = Seq.drop 1

    {-# INLINE toList #-}
    toList :: Seq a -> [a]
    toList = Foldable.toList

instance Streamable (Vector a) where
    {-# INLINE uncons #-}
    uncons :: Vector a -> Maybe (a, Vector a)
    uncons = Vector.uncons

    {-# INLINE null #-}
    null :: Vector a -> Bool
    null = Foldable.null

    {-# INLINE dropOne #-}
    dropOne :: Vector a -> Vector a
    dropOne = Vector.drop 1

    {-# INLINE toList #-}
    toList :: Vector a -> [a]
    toList = Foldable.toList

instance Streamable (SVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: SVector.Vector a -> Maybe (a, SVector.Vector a)
    uncons = SVector.uncons

    {-# INLINE null #-}
    null :: SVector.Vector a -> Bool
    null = Foldable.null

    {-# INLINE dropOne #-}
    dropOne :: SVector.Vector a -> SVector.Vector a
    dropOne = SVector.drop 1

    {-# INLINE toList #-}
    toList :: SVector.Vector a -> [a]
    toList = Foldable.toList

instance UVector.Unbox a => Streamable (UVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: UVector.Vector a -> Maybe (a, UVector.Vector a)
    uncons = UVector.uncons

    {-# INLINE null #-}
    null :: UVector.Vector a -> Bool
    null = UVector.null

    {-# INLINE dropOne #-}
    dropOne :: UVector.Vector a -> UVector.Vector a
    dropOne = UVector.drop 1

    {-# INLINE toList #-}
    toList :: UVector.Vector a -> [a]
    toList = UVector.toList

instance StVector.Storable a => Streamable (StVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: StVector.Vector a -> Maybe (a, StVector.Vector a)
    uncons = StVector.uncons

    {-# INLINE null #-}
    null :: StVector.Vector a -> Bool
    null = StVector.null

    {-# INLINE dropOne #-}
    dropOne :: StVector.Vector a -> StVector.Vector a
    dropOne = StVector.drop 1

    {-# INLINE toList #-}
    toList :: StVector.Vector a -> [a]
    toList = StVector.toList


{- | Return 'True' if @xs@ is a suffix of @ys@. -}
{-# INLINE isSuffix #-}
isSuffix :: (Streamable s, Eq (ElementOf s)) => s -> s -> Bool
isSuffix xs ys = toList xs `List.isSuffixOf` toList ys
