{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),

    -- * Basic functions.
    toList,
    isSuffixOf,
) where

-- Imports.
-- Base.
import qualified Data.Foldable as Foldable (null)
import Data.List (unfoldr)
import qualified Data.List as List (uncons, isSuffixOf)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (uncons)
import Data.Maybe (isNothing)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, uncons, null)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, uncons, null)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, uncons, null)
import qualified Data.Text as Text (Text, uncons, null)
import qualified Data.Text.Lazy as LazyText (Text, uncons, null)
import Data.Sequence (Seq (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (uncons)
import qualified Data.Vector.Strict as StrictVector (Vector, uncons)
import qualified Data.Vector.Unboxed as UnboxedVector (Vector, Unbox, uncons, null)
import qualified Data.Vector.Storable as StorableVector (Vector, Storable, uncons, null)

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


-- Instances.
instance Streamable Bytes.ByteString where
    uncons :: Bytes.ByteString -> Maybe (Word8, Bytes.ByteString)
    uncons = Bytes.uncons

    null :: Bytes.ByteString -> Bool
    null = Bytes.null

instance Streamable LazyBytes.ByteString where
    uncons :: LazyBytes.ByteString -> Maybe (Word8, LazyBytes.ByteString)
    uncons = LazyBytes.uncons

    null :: LazyBytes.ByteString -> Bool
    null = LazyBytes.null

instance Streamable ShortBytes.ShortByteString where
    uncons :: ShortBytes.ShortByteString -> Maybe (Word8, ShortBytes.ShortByteString)
    uncons = ShortBytes.uncons

    null :: ShortBytes.ShortByteString -> Bool
    null = ShortBytes.null

instance Streamable Text.Text where
    uncons :: Text.Text -> Maybe (Char, Text.Text)
    uncons = Text.uncons

    null :: Text.Text -> Bool
    null = Text.null

instance Streamable LazyText.Text where
    uncons :: LazyText.Text -> Maybe (Char, LazyText.Text)
    uncons = LazyText.uncons

    null :: LazyText.Text -> Bool
    null = LazyText.null

instance Streamable (Maybe a) where
    uncons :: Maybe a -> Maybe (a, Maybe a)
    uncons Nothing = Nothing
    uncons (Just x) = Just (x, Nothing)

    null :: Maybe a -> Bool
    null = isNothing

instance Streamable [a] where
    uncons :: [a] -> Maybe (a, [a])
    uncons = List.uncons

    null :: [a] -> Bool
    null = Foldable.null

instance Streamable (NonEmpty a) where
    uncons :: NonEmpty a -> Maybe (a, NonEmpty a)
    uncons xs =
        case NonEmpty.uncons xs of
            (_, Nothing) -> Nothing
            (y, Just ys) -> Just (y, ys)

    null :: NonEmpty a -> Bool
    null = Foldable.null

instance Streamable (Seq a) where
    uncons :: Seq a -> Maybe (a, Seq a)
    uncons Empty      = Nothing
    uncons (x :<| xs) = Just (x, xs)

    null :: Seq a -> Bool
    null = Foldable.null

instance Streamable (Vector a) where
    uncons :: Vector a -> Maybe (a, Vector a)
    uncons = Vector.uncons

    null :: Vector a -> Bool
    null = Foldable.null

instance Streamable (StrictVector.Vector a) where
    uncons :: StrictVector.Vector a -> Maybe (a, StrictVector.Vector a)
    uncons = StrictVector.uncons

    null :: StrictVector.Vector a -> Bool
    null = Foldable.null

instance UnboxedVector.Unbox a => Streamable (UnboxedVector.Vector a) where
    uncons :: UnboxedVector.Vector a -> Maybe (a, UnboxedVector.Vector a)
    uncons = UnboxedVector.uncons

    null :: UnboxedVector.Vector a -> Bool
    null = UnboxedVector.null

instance StorableVector.Storable a => Streamable (StorableVector.Vector a) where
    uncons :: StorableVector.Vector a -> Maybe (a, StorableVector.Vector a)
    uncons = StorableVector.uncons

    null :: StorableVector.Vector a -> Bool
    null = StorableVector.null


{- | Convert a 'Streamable' to a list. -}
toList :: Streamable s => s -> [ElementOf s]
toList = unfoldr uncons

{- | Return 'True' if @xs@ is a suffix of @ys@. -}
isSuffixOf :: (Streamable s, Eq (ElementOf s)) => s -> s -> Bool
isSuffixOf xs ys = toList xs `List.isSuffixOf` toList ys
