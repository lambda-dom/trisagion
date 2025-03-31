{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),

    -- * Basic functions.
    isSuffix,
) where

-- Imports.
-- Base.
import qualified Data.Foldable as Foldable (null, toList)
import Data.List (unfoldr, isSuffixOf, singleton)
import qualified Data.List as List (uncons)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (uncons)
import Data.Maybe (isNothing)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, uncons, null, unpack)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, uncons, null, unpack)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, uncons, null, unpack)
import qualified Data.Text as Text (Text, uncons, null, unpack)
import qualified Data.Text.Lazy as LazyText (Text, uncons, null, unpack)
import Data.Sequence (Seq (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (uncons)
import qualified Data.Vector.Strict as StrictVector (Vector, uncons)
import qualified Data.Vector.Unboxed as UnboxedVector (Vector, Unbox, uncons, null, toList)
import qualified Data.Vector.Storable as StorableVector (Vector, Storable, uncons, null, toList)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))


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
    null :: s -> Bool
    null = isNothing . uncons

    {- | Convert a 'Streamable' to a list. -}
    toList :: s -> [ElementOf s]
    toList = unfoldr uncons

-- Instances.
instance Streamable Bytes.ByteString where
    uncons :: Bytes.ByteString -> Maybe (Word8, Bytes.ByteString)
    uncons = Bytes.uncons

    null :: Bytes.ByteString -> Bool
    null = Bytes.null

    toList :: Bytes.ByteString -> [Word8]
    toList = Bytes.unpack

instance Streamable LazyBytes.ByteString where
    uncons :: LazyBytes.ByteString -> Maybe (Word8, LazyBytes.ByteString)
    uncons = LazyBytes.uncons

    null :: LazyBytes.ByteString -> Bool
    null = LazyBytes.null

    toList :: LazyBytes.ByteString -> [Word8]
    toList = LazyBytes.unpack

instance Streamable ShortBytes.ShortByteString where
    uncons :: ShortBytes.ShortByteString -> Maybe (Word8, ShortBytes.ShortByteString)
    uncons = ShortBytes.uncons

    null :: ShortBytes.ShortByteString -> Bool
    null = ShortBytes.null

    toList :: ShortBytes.ShortByteString -> [Word8]
    toList = ShortBytes.unpack

instance Streamable Text.Text where
    uncons :: Text.Text -> Maybe (Char, Text.Text)
    uncons = Text.uncons

    null :: Text.Text -> Bool
    null = Text.null

    toList :: Text.Text -> [Char]
    toList = Text.unpack

instance Streamable LazyText.Text where
    uncons :: LazyText.Text -> Maybe (Char, LazyText.Text)
    uncons = LazyText.uncons

    null :: LazyText.Text -> Bool
    null = LazyText.null

    toList :: LazyText.Text -> [Char]
    toList = LazyText.unpack

instance Streamable (Maybe a) where
    uncons :: Maybe a -> Maybe (a, Maybe a)
    uncons Nothing = Nothing
    uncons (Just x) = Just (x, Nothing)

    null :: Maybe a -> Bool
    null = isNothing

    toList :: Maybe a -> [a]
    toList = maybe [] singleton

instance Streamable [a] where
    uncons :: [a] -> Maybe (a, [a])
    uncons = List.uncons

    null :: [a] -> Bool
    null = Foldable.null

    toList :: [a] -> [a]
    toList = id

instance Streamable (NonEmpty a) where
    uncons :: NonEmpty a -> Maybe (a, NonEmpty a)
    uncons xs =
        case NonEmpty.uncons xs of
            (_, Nothing) -> Nothing
            (y, Just ys) -> Just (y, ys)

    null :: NonEmpty a -> Bool
    null = Foldable.null

    toList :: NonEmpty a -> [a]
    toList = Foldable.toList

instance Streamable (Seq a) where
    uncons :: Seq a -> Maybe (a, Seq a)
    uncons Empty      = Nothing
    uncons (x :<| xs) = Just (x, xs)

    null :: Seq a -> Bool
    null = Foldable.null

    toList :: Seq a -> [a]
    toList = Foldable.toList

instance Streamable (Vector a) where
    uncons :: Vector a -> Maybe (a, Vector a)
    uncons = Vector.uncons

    null :: Vector a -> Bool
    null = Foldable.null

    toList :: Vector a -> [a]
    toList = Foldable.toList

instance Streamable (StrictVector.Vector a) where
    uncons :: StrictVector.Vector a -> Maybe (a, StrictVector.Vector a)
    uncons = StrictVector.uncons

    null :: StrictVector.Vector a -> Bool
    null = Foldable.null

    toList :: StrictVector.Vector a -> [a]
    toList = Foldable.toList

instance UnboxedVector.Unbox a => Streamable (UnboxedVector.Vector a) where
    uncons :: UnboxedVector.Vector a -> Maybe (a, UnboxedVector.Vector a)
    uncons = UnboxedVector.uncons

    null :: UnboxedVector.Vector a -> Bool
    null = UnboxedVector.null

    toList :: UnboxedVector.Vector a -> [a]
    toList = UnboxedVector.toList

instance StorableVector.Storable a => Streamable (StorableVector.Vector a) where
    uncons :: StorableVector.Vector a -> Maybe (a, StorableVector.Vector a)
    uncons = StorableVector.uncons

    null :: StorableVector.Vector a -> Bool
    null = StorableVector.null

    toList :: StorableVector.Vector a -> [a]
    toList = StorableVector.toList


{- | Return 'True' if @xs@ is a suffix of @ys@. -}
isSuffix :: (Streamable s, Eq (ElementOf s)) => s -> s -> Bool
isSuffix xs ys = toList xs `isSuffixOf` toList ys
