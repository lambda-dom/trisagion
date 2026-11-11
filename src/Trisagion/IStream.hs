{- |
Module: Trisagion.IStream

The module 'Trisagion.Parser' defines the parsing monad and the module
'Trisagion.Parsers.Combinators' defines various useful parser combinators but with them alone, it
is not even possible to pull out one element of the input stream. The 'IStream' typeclass defines
the minimum requirements on the input stream @s@ to be able to do so.
-}

module Trisagion.IStream (
    -- * Typeclasses.
    IStream (..),

    -- * Functions.
    isSuffix,
) where

-- Imports.
-- Base.
import qualified Data.Foldable as Foldable (null, toList)
import Data.List (unfoldr, singleton)
import qualified Data.List as List (uncons, null, isSuffixOf)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, uncons, null, unpack, drop)
import qualified Data.ByteString.Lazy as LBytes (ByteString, uncons, null, unpack, drop)
import qualified Data.ByteString.Short as SBytes (ShortByteString, uncons, null, unpack, drop)
import qualified Data.Text as Text (Text, uncons, null, unpack, drop)
import qualified Data.Text.Lazy as LText (Text, uncons, null, unpack, drop)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq (drop, lookup)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector (uncons, drop)
import qualified Data.Vector.Strict as SVector (Vector, uncons, drop, (!?))
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, uncons, null, toList, drop, (!?))
import qualified Data.Vector.Storable as StVector (Vector, Storable, uncons, null, toList, drop, (!?))

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor)


{- | The @IStream@ typeclass for input streams.

We assume that all overriden instance implementations are extensionally equal to the default ones.
To describe the laws for the 'IStream' typeclass, we start with a definition.

__Definition__: Let @'MonoFunctor' a s@ and @'MonoFunctor' a t@ be two monofunctors. A function
@h :: s -> t@ is /mononatural/ if for every @f :: a -> a@ we have the equality

@
monomap f . h == h . monomap f
@

Since @s@ is not polymorphic we do not have free theorems to rely on, so mononaturality must be
explicitly required:

__Naturality__: The function @'uncons' :: s -> 'Maybe' (a, s)@ is mononatural.

In case it is not clear, the 'MonoFunctor' instance for @'Maybe' (a, s)@ is
@monomap f = 'fmap' (bimap f (monomap f))@.
 
__Foldability__: With the definition,

@
toList ::s -> [a]
toList = unfoldr uncons
@

and the @'Data.MonoFoldable.MonoFoldable' a s@ constraint we have the equality:

@
monotoList == toList
@

__Unconsing__: Finally, the third law says that 'uncons' is uncons-ing at the level of lists.

@
toList == maybe [] (\ (x, xs) -> x : toList xs) . uncons
@
-}
class MonoFunctor a s => IStream a s | s -> a where
    {-# MINIMAL uncons #-}

    {- | Get the first element from the source and its tail. -}
    uncons :: s -> Maybe (a, s)

    {- | Get the first element from the source.

    Default implementation is:

    @
    head = fmap fst . uncons
    @
    -}
    head :: s -> Maybe a
    head = fmap fst . uncons

    {- | Return 'True' if the source has no more elements.

    Default implementation is:

    @isNull = maybe True (const False) . uncons@
    -}
    isNull :: s -> Bool
    isNull = maybe True (const False) . uncons

    {- | Get and drop first element from the source.

    Default implementation is:

    @dropOne s = maybe s snd $ uncons s@
    -}
    dropOne :: s -> s
    dropOne s = maybe s snd $ uncons s

    {- | Convert an 'IStream' to a list.

    Default implementation is:

    @toList = unfoldr uncons@.
    -}
    toList :: s -> [a]
    toList = unfoldr uncons


-- Instances.
-- Base.
instance IStream a (Maybe a) where
    {-# INLINE uncons #-}
    uncons :: Maybe a -> Maybe (a, Maybe a)
    uncons xs =
        case xs of
            Just x -> Just (x, Nothing)
            _      -> Nothing

    {-# INLINE head #-}
    head :: Maybe a -> Maybe a
    head = id

    {-# INLINE isNull #-}
    isNull :: Maybe a -> Bool
    isNull = maybe (True) (const False)

    {-# INLINE dropOne #-}
    dropOne :: Maybe a -> Maybe a
    dropOne _ = Nothing

    {-# INLINE toList #-}
    toList :: Maybe a -> [a]
    toList = maybe [] singleton

instance IStream a [a] where
    {-# INLINE uncons #-}
    uncons :: [a] -> Maybe (a, [a])
    uncons = List.uncons

    {-# INLINE head #-}
    head :: [a] -> Maybe a
    head []      = Nothing
    head (x : _) = Just x

    {-# INLINE isNull #-}
    isNull :: [a] -> Bool
    isNull = List.null

    {-# INLINE dropOne #-}
    dropOne :: [a] -> [a]
    dropOne = drop 1

    {-# INLINE toList #-}
    toList :: [a] -> [a]
    toList = id


-- Libraries.
instance IStream Word8 Bytes.ByteString where
    {-# INLINE uncons #-}
    uncons :: Bytes.ByteString -> Maybe (Word8, Bytes.ByteString)
    uncons = Bytes.uncons

    {-# INLINE isNull #-}
    isNull :: Bytes.ByteString -> Bool
    isNull = Bytes.null

    {-# INLINE dropOne #-}
    dropOne :: Bytes.ByteString -> Bytes.ByteString
    dropOne = Bytes.drop 1

    {-# INLINE toList #-}
    toList :: Bytes.ByteString -> [Word8]
    toList = Bytes.unpack

instance IStream Word8 LBytes.ByteString where
    {-# INLINE uncons #-}
    uncons :: LBytes.ByteString -> Maybe (Word8, LBytes.ByteString)
    uncons = LBytes.uncons

    {-# INLINE isNull #-}
    isNull :: LBytes.ByteString -> Bool
    isNull = LBytes.null

    {-# INLINE dropOne #-}
    dropOne :: LBytes.ByteString -> LBytes.ByteString
    dropOne = LBytes.drop 1

    {-# INLINE toList #-}
    toList :: LBytes.ByteString -> [Word8]
    toList = LBytes.unpack

instance IStream Word8 SBytes.ShortByteString where
    {-# INLINE uncons #-}
    uncons :: SBytes.ShortByteString -> Maybe (Word8, SBytes.ShortByteString)
    uncons = SBytes.uncons

    {-# INLINE isNull #-}
    isNull :: SBytes.ShortByteString -> Bool
    isNull = SBytes.null

    {-# INLINE dropOne #-}
    dropOne :: SBytes.ShortByteString -> SBytes.ShortByteString
    dropOne = SBytes.drop 1

    {-# INLINE toList #-}
    toList :: SBytes.ShortByteString -> [Word8]
    toList = SBytes.unpack

instance IStream Char Text.Text where
    {-# INLINE uncons #-}
    uncons :: Text.Text -> Maybe (Char, Text.Text)
    uncons = Text.uncons

    {-# INLINE isNull #-}
    isNull :: Text.Text -> Bool
    isNull = Text.null

    {-# INLINE dropOne #-}
    dropOne :: Text.Text -> Text.Text
    dropOne = Text.drop 1

    {-# INLINE toList #-}
    toList :: Text.Text -> [Char]
    toList = Text.unpack

instance IStream Char LText.Text where
    {-# INLINE uncons #-}
    uncons :: LText.Text -> Maybe (Char, LText.Text)
    uncons = LText.uncons

    {-# INLINE isNull #-}
    isNull :: LText.Text -> Bool
    isNull = LText.null

    {-# INLINE dropOne #-}
    dropOne :: LText.Text -> LText.Text
    dropOne = LText.drop 1

    {-# INLINE toList #-}
    toList :: LText.Text -> [Char]
    toList = LText.unpack

instance IStream a (Seq a) where
    {-# INLINE uncons #-}
    uncons :: Seq a -> Maybe (a, Seq a)
    uncons Empty      = Nothing
    uncons (x :<| xs) = Just (x, xs)

    {-# INLINE head #-}
    head :: Seq a -> Maybe a
    head = Seq.lookup 0

    {-# INLINE isNull #-}
    isNull :: Seq a -> Bool
    isNull = Foldable.null

    {-# INLINE dropOne #-}
    dropOne :: Seq a -> Seq a
    dropOne = Seq.drop 1

    {-# INLINE toList #-}
    toList :: Seq a -> [a]
    toList = Foldable.toList

instance IStream a (Vector a) where
    {-# INLINE uncons #-}
    uncons :: Vector a -> Maybe (a, Vector a)
    uncons = Vector.uncons

    {-# INLINE head #-}
    head :: Vector a -> Maybe a
    head = (!? 0)

    {-# INLINE isNull #-}
    isNull :: Vector a -> Bool
    isNull = Foldable.null

    {-# INLINE dropOne #-}
    dropOne :: Vector a -> Vector a
    dropOne = Vector.drop 1

    {-# INLINE toList #-}
    toList :: Vector a -> [a]
    toList = Foldable.toList

instance IStream a (SVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: SVector.Vector a -> Maybe (a, SVector.Vector a)
    uncons = SVector.uncons

    {-# INLINE head #-}
    head :: SVector.Vector a -> Maybe a
    head = (SVector.!? 0)

    {-# INLINE isNull #-}
    isNull :: SVector.Vector a -> Bool
    isNull = Foldable.null

    {-# INLINE dropOne #-}
    dropOne :: SVector.Vector a -> SVector.Vector a
    dropOne = SVector.drop 1

    {-# INLINE toList #-}
    toList :: SVector.Vector a -> [a]
    toList = Foldable.toList

instance UVector.Unbox a => IStream a (UVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: UVector.Vector a -> Maybe (a, UVector.Vector a)
    uncons = UVector.uncons

    {-# INLINE head #-}
    head :: UVector.Vector a -> Maybe a
    head = (UVector.!? 0)

    {-# INLINE isNull #-}
    isNull :: UVector.Vector a -> Bool
    isNull = UVector.null

    {-# INLINE dropOne #-}
    dropOne :: UVector.Vector a -> UVector.Vector a
    dropOne = UVector.drop 1

    {-# INLINE toList #-}
    toList :: UVector.Vector a -> [a]
    toList = UVector.toList

instance StVector.Storable a => IStream a (StVector.Vector a) where
    {-# INLINE uncons #-}
    uncons :: StVector.Vector a -> Maybe (a, StVector.Vector a)
    uncons = StVector.uncons

    {-# INLINE head #-}
    head :: StVector.Vector a -> Maybe a
    head = (StVector.!? 0)

    {-# INLINE isNull #-}
    isNull :: StVector.Vector a -> Bool
    isNull = StVector.null

    {-# INLINE dropOne #-}
    dropOne :: StVector.Vector a -> StVector.Vector a
    dropOne = StVector.drop 1

    {-# INLINE toList #-}
    toList :: StVector.Vector a -> [a]
    toList = StVector.toList


{- | Return 'True' if @xs@ is a suffix of @ys@.

Default implementation is:

@
'Data.List.isSuffixOf' (toList xs) (toList ys)
@
-}
{-# INLINE isSuffix #-}
isSuffix :: (IStream a s, Eq a) => s -> s -> Bool
isSuffix xs ys = toList xs `List.isSuffixOf` toList ys
