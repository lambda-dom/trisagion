{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (head, tail)

-- Base.
import qualified Data.Foldable as Foldable (null)
import Data.List (unfoldr, singleton)
import qualified Data.List as List (uncons, isSuffixOf)
import Data.Maybe (isNothing)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, uncons, null)
import qualified Data.ByteString.Lazy as LBytes (ByteString, uncons, null)
import qualified Data.ByteString.Short as SBytes (ShortByteString, uncons, null)
import qualified Data.Text as Text (Text, uncons, null)
import qualified Data.Text.Lazy as LText (Text, uncons, null)
import Data.Sequence (Seq (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (uncons)
import qualified Data.Vector.Strict as SVector (Vector, uncons)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, uncons, null)
import qualified Data.Vector.Storable as StVector (Vector, Storable, uncons, null)

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

    {- | Return the tail of the stream. -}
    {-# INLINE dropOne #-}
    dropOne :: s -> Maybe s
    dropOne = fmap snd . uncons

    {- | Convert a 'Streamable' to a list. -}
    {-# INLINE toList #-}
    toList :: s -> [ElementOf s]
    toList = unfoldr uncons

    {- | Return 'True' if @xs@ is a suffix of @ys@. -}
    {-# INLINE isSuffix #-}
    isSuffix :: Eq (ElementOf s) => s -> s -> Bool
    isSuffix xs ys = toList xs `List.isSuffixOf` toList ys

-- Instances.
instance Streamable Bytes.ByteString where
    {-# INLINE uncons #-}
    uncons :: Bytes.ByteString -> Maybe (Word8, Bytes.ByteString)
    uncons = Bytes.uncons

    {-# INLINE null #-}
    null :: Bytes.ByteString -> Bool
    null = Bytes.null

instance Streamable LBytes.ByteString where
    {-# INLINE uncons #-}
    uncons :: LBytes.ByteString -> Maybe (Word8, LBytes.ByteString)
    uncons = LBytes.uncons

    {-# INLINE null #-}
    null :: LBytes.ByteString -> Bool
    null = LBytes.null

instance Streamable SBytes.ShortByteString where
    {-# INLINE uncons #-}
    uncons :: SBytes.ShortByteString -> Maybe (Word8, SBytes.ShortByteString)
    uncons = SBytes.uncons

    {-# INLINE null #-}
    null :: SBytes.ShortByteString -> Bool
    null = SBytes.null

instance Streamable Text.Text where
    {-# INLINE uncons #-}
    uncons :: Text.Text -> Maybe (Char, Text.Text)
    uncons = Text.uncons

    {-# INLINE null #-}
    null :: Text.Text -> Bool
    null = Text.null

instance Streamable LText.Text where
    {-# INLINE uncons #-}
    uncons :: LText.Text -> Maybe (Char, LText.Text)
    uncons = LText.uncons

    {-# INLINE null #-}
    null :: LText.Text -> Bool
    null = LText.null

instance Streamable (Maybe a) where
    {-# INLINE uncons #-}
    uncons :: Maybe a -> Maybe (a, Maybe a)
    uncons Nothing = Nothing
    uncons (Just x) = Just (x, Nothing)

    {-# INLINE null #-}
    null :: Maybe a -> Bool
    null = isNothing

    {-# INLINE dropOne #-}
    dropOne :: Maybe a -> Maybe (Maybe a)
    dropOne Nothing  = Nothing
    dropOne (Just _) = Just Nothing

    {-# INLINE toList #-}
    toList :: Maybe a -> [a]
    toList = maybe [] singleton

    {-# INLINE isSuffix #-}
    isSuffix :: Eq a => Maybe a -> Maybe a -> Bool
    isSuffix Nothing  _        = True
    isSuffix (Just x) (Just y) = x == y
    isSuffix _        _        = False

instance Streamable [a] where
    {-# INLINE uncons #-}
    uncons :: [a] -> Maybe (a, [a])
    uncons = List.uncons

    {-# INLINE null #-}
    null :: [a] -> Bool
    null = Foldable.null

    {-# INLINE dropOne #-}
    dropOne :: [a] -> Maybe [a]
    dropOne []       = Nothing
    dropOne (_ : xs) = Just xs

    {-# INLINE toList #-}
    toList :: [a] -> [a]
    toList = id

    {-# INLINE isSuffix #-}
    isSuffix = List.isSuffixOf

instance Streamable (Seq a) where
    {-# INLINE uncons #-}
    uncons :: Seq a -> Maybe (a, Seq a)
    uncons Empty      = Nothing
    uncons (x :<| xs) = Just (x, xs)

    {-# INLINE null #-}
    null :: Seq a -> Bool
    null = Foldable.null

instance Streamable (Vector a) where
    {-# INLINE uncons #-}
    uncons :: Vector a -> Maybe (a, Vector a)
    uncons = Vector.uncons

    {-# INLINE null #-}
    null :: Vector a -> Bool
    null = Foldable.null

instance Streamable (SVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: SVector.Vector a -> Maybe (a, SVector.Vector a)
    uncons = SVector.uncons

    {-# INLINE null #-}
    null :: SVector.Vector a -> Bool
    null = Foldable.null

instance UVector.Unbox a => Streamable (UVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: UVector.Vector a -> Maybe (a, UVector.Vector a)
    uncons = UVector.uncons

    {-# INLINE null #-}
    null :: UVector.Vector a -> Bool
    null = UVector.null

instance StVector.Storable a => Streamable (StVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: StVector.Vector a -> Maybe (a, StVector.Vector a)
    uncons = StVector.uncons

    {-# INLINE null #-}
    null :: StVector.Vector a -> Bool
    null = StVector.null
