{- |
Module: Trisagion.Typeclasses.Split

The 'Source' typeclass allows us to, in principle, write all the needed parsers, but the
implementations can be very inefficient. To address that, the 'Split' typeclass adds new
primitives.
-}

module Trisagion.Typeclasses.Split (
    -- * Typeclasses.
    Split (..),
) where

-- Imports.
-- Base.
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, span, splitAt, empty, drop, dropWhile, length, stripPrefix)
import qualified Data.ByteString.Lazy as LBytes (ByteString, span, splitAt, empty, drop, dropWhile, length, stripPrefix)
import qualified Data.ByteString.Short as SBytes (ShortByteString, span, splitAt, empty, drop, dropWhile, length, stripPrefix)
import qualified Data.Text as Text (Text, span, splitAt, empty, drop, dropWhile, stripPrefix, length)
import qualified Data.Text.Lazy as LText (Text, span, splitAt, empty, drop, dropWhile, stripPrefix, length)
import Data.Sequence (Seq, ViewL (..), empty, viewl, (<|))
import qualified Data.Sequence as Seq (spanl, splitAt, drop, dropWhileL)
import qualified Data.Vector as Vector (Vector, span, splitAt, empty, drop, dropWhile)
import qualified Data.Vector.Strict as SVector (Vector, span, splitAt, empty, drop, dropWhile)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, span, splitAt, empty, drop, dropWhile, length)
import qualified Data.Vector.Storable as StVector (Vector, Storable, span, splitAt, empty, drop, dropWhile, length)

-- Package.
import qualified Trisagion.Utils.List as List (splitAtExact, matchPrefix)
import Trisagion.Typeclasses.Source (Source)


-- $setup
-- >>> import Data.Bifunctor
-- >>> import Mono.Typeclasses.MonoFunctor


{- | The @Split@ typeclass.

Mirroring the laws for the 'Source' typeclass, the first law is:

__Mononaturality__: With the constraints @('Mono.Typeclasses.MonoFunctor' a s,
'Mono.Typeclasses.MonoFunctor' a b)@, @singleton@ and @splitPrefix n@ are mononatural.

__List identities__: For the second law, assuming a
@'Mono.Typeclasses.MonoFoldable.MonoFoldable' a b@ constraint on the prefix, then at the level of
lists @splitPrefix@ is 'Data.List.splitAt' and @splitWith@ is 'Data.List.span':

@
bimap monotoList toList . splitPrefix n == splitAt n . toList
bimap monotoList toList . splitWith p == span p . toList
@

__Compatibility__: The third and final law is a compatibility condition between
'Trisagion.Typeclasses.Streamable.uncons' and @splitPrefix@:

@
maybe ([], []) (bimap singleton toList) . uncons == bimap monotoList toList . splitPrefix 1
@

=== __Counterexample:__

The following example shows that @'splitWith' p@ is /not/ mononatural.

>>> let p = ('\NUL' ==)
>>> let f = const '\NUL'
>>> splitWith p . monomap f $ "a"
("\NUL","")
>>> bimap (monomap f) (monomap f) . splitWith p $ "a"
("","\NUL")
-}
class Source a s => Split a b s | s -> b where
    {-# MINIMAL splitPrefix, splitWith, splitPrefixExact, matchPrefix #-}

    {- | Split the stream at offset @n@ into a pair @(prefix, suffix)@. -}
    splitPrefix :: Int -> s -> (b, s)

    {- | Split the longest prefix from the stream whose elements satisfy a predicate. -}
    splitWith :: (a -> Bool) -> s -> (b, s)

    {- | Split a prefix of exact size. -}
    splitPrefixExact :: Int -> s -> Maybe (b, s)

    {- | Parse and drop a matching prefix. -}
    matchPrefix :: b -> s -> Maybe s

    {- | Return the remainder of the stream as a prefix.

    Default implementation is:

    @splitRemainder = splitWith (const True)@

    Instances can (almost) always define a faster implementation using a nullary operation
    @empty :: s@.
    -}
    splitRemainder :: s -> (b, s)
    splitRemainder = splitWith (const True)

    {- | Drop a fixed-size prefix from the stream.

    Default implementation is:

    @dropPrefix n = snd . splitPrefix n@
    -}
    dropPrefix :: Int -> s -> s
    dropPrefix n = snd . splitPrefix n

    {- | Drop the longest prefix satisfying a predicate from the stream.

    Default implementation is:

    @dropWith = snd . splitWith p@
    -}
    dropWith :: (a -> Bool) -> s -> s
    dropWith p = snd . splitWith p


-- Instances.
-- Base.
instance Eq a => Split a [a] [a] where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> [a] -> ([a], [a])
    splitPrefix n = splitAt n

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith p = span p

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> [a] -> Maybe ([a], [a])
    splitPrefixExact n = List.splitAtExact n

    {-# INLINE matchPrefix #-}
    matchPrefix :: [a] -> [a] -> Maybe [a]
    matchPrefix xs = List.matchPrefix xs

    {-# INLINE splitRemainder #-}
    splitRemainder :: [a] -> ([a], [a])
    splitRemainder xs = (xs, [])

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> [a] -> [a]
    dropPrefix = drop

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> [a] -> [a]
    dropWith = dropWhile


-- Libraries.
instance Split Word8 Bytes.ByteString Bytes.ByteString where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitPrefix = Bytes.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitWith = Bytes.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> Bytes.ByteString -> Maybe (Bytes.ByteString, Bytes.ByteString)
    splitPrefixExact n xs = if Bytes.length xs < n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Bytes.ByteString -> Bytes.ByteString -> Maybe Bytes.ByteString
    matchPrefix xs ys = Bytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitRemainder s = (s, Bytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> Bytes.ByteString -> Bytes.ByteString
    dropPrefix = Bytes.drop

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> Bytes.ByteString -> Bytes.ByteString
    dropWith = Bytes.dropWhile

instance Split Word8 LBytes.ByteString LBytes.ByteString where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitPrefix = LBytes.splitAt . fromIntegral

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitWith = LBytes.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> LBytes.ByteString -> Maybe (LBytes.ByteString, LBytes.ByteString)
    splitPrefixExact n xs = if LBytes.length xs < fromIntegral n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: LBytes.ByteString -> LBytes.ByteString -> Maybe LBytes.ByteString
    matchPrefix xs ys = LBytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitRemainder s = (s, LBytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> LBytes.ByteString -> LBytes.ByteString
    dropPrefix = LBytes.drop . fromIntegral

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> LBytes.ByteString -> LBytes.ByteString
    dropWith = LBytes.dropWhile

instance Split Word8 SBytes.ShortByteString SBytes.ShortByteString where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitPrefix = SBytes.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitWith = SBytes.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> SBytes.ShortByteString -> Maybe (SBytes.ShortByteString, SBytes.ShortByteString)
    splitPrefixExact n xs = if SBytes.length xs < n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: SBytes.ShortByteString -> SBytes.ShortByteString -> Maybe SBytes.ShortByteString
    matchPrefix xs ys = SBytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitRemainder s = (s, SBytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> SBytes.ShortByteString -> SBytes.ShortByteString
    dropPrefix = SBytes.drop

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> SBytes.ShortByteString
    dropWith = SBytes.dropWhile

instance Split Char Text.Text Text.Text where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> Text.Text -> (Text.Text, Text.Text)
    splitPrefix = Text.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> Text.Text -> Maybe (Text.Text, Text.Text)
    splitPrefixExact n xs = if Text.length xs < n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Text.Text -> Text.Text -> Maybe Text.Text
    matchPrefix xs ys = Text.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: Text.Text -> (Text.Text, Text.Text)
    splitRemainder s = (s, Text.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> Text.Text -> Text.Text
    dropPrefix = Text.drop

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> Text.Text -> Text.Text
    dropWith = Text.dropWhile

instance Split Char LText.Text LText.Text where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> LText.Text -> (LText.Text, LText.Text)
    splitPrefix = LText.splitAt . fromIntegral

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> LText.Text -> (LText.Text, LText.Text)
    splitWith = LText.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> LText.Text -> Maybe (LText.Text, LText.Text)
    splitPrefixExact n xs = if LText.length xs < fromIntegral n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: LText.Text -> LText.Text -> Maybe LText.Text
    matchPrefix xs ys = LText.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: LText.Text -> (LText.Text, LText.Text)
    splitRemainder s = (s, LText.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> LText.Text -> LText.Text
    dropPrefix = LText.drop . fromIntegral

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> LText.Text -> LText.Text
    dropWith = LText.dropWhile

instance Eq a => Split a (Seq a) (Seq a) where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> Seq a -> (Seq a, Seq a)
    splitPrefix = Seq.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    splitWith = Seq.spanl

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> Seq a -> Maybe (Seq a, Seq a)
    splitPrefixExact n xs = if length xs < n then Nothing else Just (splitPrefix n xs)

    {-# INLINEABLE matchPrefix #-}
    matchPrefix :: Seq a -> Seq a -> Maybe (Seq a)
    matchPrefix xs ys =
        case viewl xs of
            EmptyL      -> Just xs
            (x :< xs') -> case viewl ys of
                EmptyL      -> Nothing
                (y :< ys') -> if x == y then fmap (x <|) $ matchPrefix xs' ys' else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: Seq a -> (Seq a, Seq a)
    splitRemainder xs = (xs, empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> Seq a -> Seq a
    dropPrefix = Seq.drop

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> Seq a -> Seq a
    dropWith = Seq.dropWhileL

instance Eq a => Split a (Vector.Vector a) (Vector.Vector a) where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitPrefix = Vector.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitWith = Vector.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> Vector.Vector a -> Maybe (Vector.Vector a, Vector.Vector a)
    splitPrefixExact n xs = if length xs < n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Vector.Vector a -> Vector.Vector a -> Maybe (Vector.Vector a)
    matchPrefix xs ys =
        if length xs > length ys then Nothing else
            let (prefix, rest) = Vector.splitAt (length xs) ys in
                if xs == prefix then Just rest else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitRemainder xs = (xs, Vector.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> Vector.Vector a -> Vector.Vector a
    dropPrefix = Vector.drop

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> Vector.Vector a -> Vector.Vector a
    dropWith = Vector.dropWhile

instance Eq a => Split a (SVector.Vector a) (SVector.Vector a) where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitPrefix = SVector.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitWith  = SVector.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> SVector.Vector a -> Maybe (SVector.Vector a, SVector.Vector a)
    splitPrefixExact n xs = if length xs < n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: SVector.Vector a -> SVector.Vector a -> Maybe (SVector.Vector a)
    matchPrefix xs ys =
        if length xs > length ys then Nothing else
            let (prefix, rest) = SVector.splitAt (length xs) ys in
                if xs == prefix then Just rest else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitRemainder xs = (xs, SVector.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> SVector.Vector a -> SVector.Vector a
    dropPrefix = SVector.drop

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> SVector.Vector a -> SVector.Vector a
    dropWith = SVector.dropWhile

instance (Eq a, UVector.Unbox a) => Split a (UVector.Vector a) (UVector.Vector a) where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitPrefix = UVector.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitWith = UVector.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> UVector.Vector a -> Maybe (UVector.Vector a, UVector.Vector a)
    splitPrefixExact n xs = if UVector.length xs < n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: UVector.Vector a -> UVector.Vector a -> Maybe (UVector.Vector a)
    matchPrefix xs ys =
        if UVector.length xs > UVector.length ys then Nothing else
            let (prefix, rest) = UVector.splitAt (UVector.length xs) ys in
                if xs == prefix then Just rest else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitRemainder xs = (xs, UVector.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> UVector.Vector a -> UVector.Vector a
    dropPrefix = UVector.drop

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> UVector.Vector a -> UVector.Vector a
    dropWith = UVector.dropWhile

instance (Eq a, StVector.Storable a) => Split a (StVector.Vector a) (StVector.Vector a) where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Int -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitPrefix = StVector.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitWith = StVector.span

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Int -> StVector.Vector a -> Maybe (StVector.Vector a, StVector.Vector a)
    splitPrefixExact n xs = if StVector.length xs < n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: StVector.Vector a -> StVector.Vector a -> Maybe (StVector.Vector a)
    matchPrefix xs ys =
        if StVector.length xs > StVector.length ys then Nothing else
            let (prefix, rest) = StVector.splitAt (StVector.length xs) ys in
                if xs == prefix then Just rest else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitRemainder xs = (xs, StVector.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Int -> StVector.Vector a -> StVector.Vector a
    dropPrefix = StVector.drop

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> StVector.Vector a -> StVector.Vector a
    dropWith = StVector.dropWhile
