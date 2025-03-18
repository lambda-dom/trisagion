{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),

    -- * Basic functions.
    toList,
    isSuffix,
) where


-- Imports.
-- Base.
import Data.List (unfoldr, isSuffixOf)
import qualified Data.List as List (uncons)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (uncons)
import Data.Maybe (isNothing)
import Data.Word (Word8)

-- Libraries.
import Data.Sequence (Seq (..))
import qualified Data.Text as Text (Text, uncons, null)
import qualified Data.Text.Lazy as LazyText (Text, uncons, null)
import qualified Data.ByteString as Bytes (ByteString, uncons, null)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, uncons, null)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, uncons, null)
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

__Naturality__: The function @'splitOne' :: s -> 'Maybe' ('ElementOf' s, s)@ is natural.

In case it is not clear, the 'MonoFunctor' instance for @'Maybe' ('ElementOf' s, s)@ is:

@
monomap :: ('ElementOf' s -> 'ElementOf' s) -> 'Maybe' ('ElementOf' s, s) -> 'Maybe' ('ElementOf' s, s)
monomap f = fmap (bimap f (monomap f))
@
 
Given @'splitOne' :: s -> 'Maybe' ('ElementOf' s, s)@ we can define @'toList' ::s -> [ElementOf s]@
by @'Data.List.unfoldr' 'splitOne'@.

__Foldability__:

prop> MonoFoldable s => monotoList = toList

Finally, the third law says that 'splitOne' really is uncons-ing at the level of lists.

__Unconsing__:

prop> toList = maybe [] (\ (x, xs) -> x : toList xs) . splitOne
-}
class MonoFunctor s => Streamable s where
    {-# MINIMAL splitOne #-}

    {- | Get, or uncons, the first element from the streamable. -}
    splitOne :: s -> Maybe (ElementOf s, s)

    {- | Return 'True' if there are no elements in the input stream. -}
    isNull :: s -> Bool
    isNull = isNothing . splitOne


{- | Convert a 'Streamable' to a list. -}
toList :: Streamable s => s -> [ElementOf s]
toList = unfoldr splitOne

{- | Return 'True' if @xs@ is a suffix of @ys@. -}
isSuffix :: (Streamable s, Eq (ElementOf s)) => s -> s -> Bool
isSuffix xs ys = toList xs `isSuffixOf` toList ys


-- Instances.
instance Streamable Bytes.ByteString where
    splitOne :: Bytes.ByteString -> Maybe (Word8, Bytes.ByteString)
    splitOne = Bytes.uncons

    isNull :: Bytes.ByteString -> Bool
    isNull = Bytes.null

instance Streamable LazyBytes.ByteString where
    splitOne :: LazyBytes.ByteString -> Maybe (Word8, LazyBytes.ByteString)
    splitOne = LazyBytes.uncons

    isNull :: LazyBytes.ByteString -> Bool
    isNull = LazyBytes.null

instance Streamable ShortBytes.ShortByteString where
    splitOne :: ShortBytes.ShortByteString -> Maybe (Word8, ShortBytes.ShortByteString)
    splitOne = ShortBytes.uncons

    isNull :: ShortBytes.ShortByteString -> Bool
    isNull = ShortBytes.null

instance Streamable Text.Text where
    splitOne :: Text.Text -> Maybe (Char, Text.Text)
    splitOne = Text.uncons

    isNull :: Text.Text -> Bool
    isNull = Text.null

instance Streamable LazyText.Text where
    splitOne :: LazyText.Text -> Maybe (Char, LazyText.Text)
    splitOne = LazyText.uncons

    isNull :: LazyText.Text -> Bool
    isNull = LazyText.null

instance Streamable [a] where
    splitOne :: [a] -> Maybe (a, [a])
    splitOne = List.uncons

    isNull :: [a] -> Bool
    isNull = null

instance Streamable (NonEmpty a) where
    splitOne :: NonEmpty a -> Maybe (a, NonEmpty a)
    splitOne xs =
        case NonEmpty.uncons xs of
            (_, Nothing) -> Nothing
            (y, Just ys) -> Just (y, ys)

    isNull :: NonEmpty a -> Bool
    isNull = null

instance Streamable (Seq a) where
    splitOne :: Seq a -> Maybe (a, Seq a)
    splitOne Empty      = Nothing
    splitOne (x :<| xs) = Just (x, xs)

    isNull :: Seq a -> Bool
    isNull = null

instance Streamable (Vector a) where
    splitOne :: Vector a -> Maybe (a, Vector a)
    splitOne = Vector.uncons

    isNull :: Vector a -> Bool
    isNull = null

instance Streamable (StrictVector.Vector a) where
    splitOne :: StrictVector.Vector a -> Maybe (a, StrictVector.Vector a)
    splitOne = StrictVector.uncons

    isNull :: StrictVector.Vector a -> Bool
    isNull = null

instance UnboxedVector.Unbox a => Streamable (UnboxedVector.Vector a) where
    splitOne :: UnboxedVector.Vector a -> Maybe (a, UnboxedVector.Vector a)
    splitOne = UnboxedVector.uncons

    isNull :: UnboxedVector.Vector a -> Bool
    isNull = UnboxedVector.null

instance StorableVector.Storable a => Streamable (StorableVector.Vector a) where
    splitOne :: StorableVector.Vector a -> Maybe (a, StorableVector.Vector a)
    splitOne = StorableVector.uncons

    isNull :: StorableVector.Vector a -> Bool
    isNull = StorableVector.null
