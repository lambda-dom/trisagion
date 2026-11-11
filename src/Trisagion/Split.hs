{- |
Module: Trisagion.Split

The 'IStream' typeclass allows us to, in principle, write all the needed parsers, but the
implementations can be very inefficient. To address that, the 'Split' typeclass adds new
primitives.
-}

module Trisagion.Split (
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
import Trisagion.IStream (IStream)


-- $setup
-- >>> import Data.Bifunctor
-- >>> import Data.MonoFunctor


{- | The @Split@ typeclass.

Mirroring the laws for the 'IStream' typeclass, the first law is:

__Mononaturality__: With the constraints @('Data.MonoFunctor.MonoFunctor' a s,
'Data.MonoFunctor.MonoFunctor' a b)@, @singleton@ and @splitPrefix n@ are mononatural.

__List identities__: For the second law, assuming a
@'Data.MonoFoldable.MonoFoldable' a b@ constraint on the prefix, then at the level of
lists @splitPrefix@ is 'Data.List.splitAt' and @splitWith@ is 'Data.List.span':

@
bimap monotoList toList . splitPrefix n == splitAt n . toList
bimap monotoList toList . splitWith p == span p . toList
@

__Compatibility__: The third and final law is a compatibility condition between
'Trisagion.IStream.uncons' and @splitPrefix@:

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
class IStream a s => Split a b s | s -> b where
    {-# MINIMAL splitN, splitWith, splitExact, matchPrefix #-}

    {- | Split the stream at offset @n@ into a pair @(prefix, suffix)@. -}
    splitN :: Int -> s -> (b, s)

    {- | Split the longest prefix from the stream whose elements satisfy a predicate. -}
    splitWith :: (a -> Bool) -> s -> (b, s)

    {- | Split a prefix of exact size. -}
    splitExact :: Int -> s -> Maybe (b, s)

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

    @dropN n = snd . splitPrefix n@
    -}
    dropN :: Int -> s -> s
    dropN n = snd . splitN n

    {- | Drop an exact, fixed-size prefix from the stream.

    Default implementation is:

    @dropExact n = fmap snd . splitPrefixExact n@
    -}
    dropExact :: Int -> s -> Maybe s
    dropExact n = fmap snd . splitExact n

    {- | Drop the longest prefix satisfying a predicate from the stream.

    Default implementation is:

    @dropWith = snd . splitWith p@
    -}
    dropWith :: (a -> Bool) -> s -> s
    dropWith p = snd . splitWith p


-- Instances.
-- Base.
instance Eq a => Split a [a] [a] where
    {-# INLINE splitN #-}
    splitN :: Int -> [a] -> ([a], [a])
    splitN = splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith = span

    {-# INLINE splitExact #-}
    splitExact :: Int -> [a] -> Maybe ([a], [a])
    splitExact = List.splitAtExact

    {-# INLINE matchPrefix #-}
    matchPrefix :: [a] -> [a] -> Maybe [a]
    matchPrefix = List.matchPrefix

    {-# INLINE splitRemainder #-}
    splitRemainder :: [a] -> ([a], [a])
    splitRemainder xs = (xs, [])

    {-# INLINE dropN #-}
    dropN :: Int -> [a] -> [a]
    dropN = drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> [a] -> Maybe [a]
    dropExact n xs
        | n <= 0    = Just xs
        | otherwise =
            case xs of
                []       -> Nothing
                (_ : ys) -> dropExact (pred n) ys

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> [a] -> [a]
    dropWith = dropWhile


-- Libraries.
instance Split Word8 Bytes.ByteString Bytes.ByteString where
    {-# INLINE splitN #-}
    splitN :: Int -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitN = Bytes.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitWith = Bytes.span

    {-# INLINE splitExact #-}
    splitExact :: Int -> Bytes.ByteString -> Maybe (Bytes.ByteString, Bytes.ByteString)
    splitExact n xs =
        if Bytes.length xs < n
        then Nothing
        else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Bytes.ByteString -> Bytes.ByteString -> Maybe Bytes.ByteString
    matchPrefix xs ys = Bytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitRemainder s = (s, Bytes.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> Bytes.ByteString -> Bytes.ByteString
    dropN = Bytes.drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> Bytes.ByteString -> Maybe Bytes.ByteString
    dropExact n xs =
        if Bytes.length xs < n
        then Nothing
        else Just $ dropN n xs

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> Bytes.ByteString -> Bytes.ByteString
    dropWith = Bytes.dropWhile

instance Split Word8 LBytes.ByteString LBytes.ByteString where
    {-# INLINE splitN #-}
    splitN :: Int -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitN = LBytes.splitAt . fromIntegral

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitWith = LBytes.span

    {-# INLINE splitExact #-}
    splitExact :: Int -> LBytes.ByteString -> Maybe (LBytes.ByteString, LBytes.ByteString)
    splitExact n xs =
        if LBytes.length xs < fromIntegral n
        then Nothing
        else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: LBytes.ByteString -> LBytes.ByteString -> Maybe LBytes.ByteString
    matchPrefix xs ys = LBytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitRemainder s = (s, LBytes.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> LBytes.ByteString -> LBytes.ByteString
    dropN = LBytes.drop . fromIntegral

    {-# INLINE dropExact #-}
    dropExact :: Int -> LBytes.ByteString -> Maybe LBytes.ByteString
    dropExact n xs =
        if LBytes.length xs < fromIntegral n
        then Nothing
        else Just $ dropN n xs

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> LBytes.ByteString -> LBytes.ByteString
    dropWith = LBytes.dropWhile

instance Split Word8 SBytes.ShortByteString SBytes.ShortByteString where
    {-# INLINE splitN #-}
    splitN
        :: Int
        -> SBytes.ShortByteString
        -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitN = SBytes.splitAt

    {-# INLINE splitWith #-}
    splitWith
        :: (Word8 -> Bool)
        -> SBytes.ShortByteString
        -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitWith = SBytes.span

    {-# INLINE splitExact #-}
    splitExact
        :: Int
        -> SBytes.ShortByteString
        -> Maybe (SBytes.ShortByteString, SBytes.ShortByteString)
    splitExact n xs =
        if SBytes.length xs < n
        then Nothing
        else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix
        :: SBytes.ShortByteString
        -> SBytes.ShortByteString
        -> Maybe SBytes.ShortByteString
    matchPrefix xs ys = SBytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitRemainder s = (s, SBytes.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> SBytes.ShortByteString -> SBytes.ShortByteString
    dropN = SBytes.drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> SBytes.ShortByteString -> Maybe SBytes.ShortByteString
    dropExact n xs =
        if SBytes.length xs < n
        then Nothing
        else Just $ dropN n xs

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> SBytes.ShortByteString
    dropWith = SBytes.dropWhile

instance Split Char Text.Text Text.Text where
    {-# INLINE splitN #-}
    splitN :: Int -> Text.Text -> (Text.Text, Text.Text)
    splitN = Text.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

    {-# INLINE splitExact #-}
    splitExact :: Int -> Text.Text -> Maybe (Text.Text, Text.Text)
    splitExact n xs = if Text.length xs < n then Nothing else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Text.Text -> Text.Text -> Maybe Text.Text
    matchPrefix xs ys = Text.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: Text.Text -> (Text.Text, Text.Text)
    splitRemainder s = (s, Text.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> Text.Text -> Text.Text
    dropN = Text.drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> Text.Text -> Maybe Text.Text
    dropExact n xs =
        if Text.length xs < n
        then Nothing
        else Just $ Text.drop n xs

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> Text.Text -> Text.Text
    dropWith = Text.dropWhile

instance Split Char LText.Text LText.Text where
    {-# INLINE splitN #-}
    splitN :: Int -> LText.Text -> (LText.Text, LText.Text)
    splitN = LText.splitAt . fromIntegral

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> LText.Text -> (LText.Text, LText.Text)
    splitWith = LText.span

    {-# INLINE splitExact #-}
    splitExact :: Int -> LText.Text -> Maybe (LText.Text, LText.Text)
    splitExact n xs
        = if LText.length xs < fromIntegral n
        then Nothing
        else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: LText.Text -> LText.Text -> Maybe LText.Text
    matchPrefix xs ys = LText.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: LText.Text -> (LText.Text, LText.Text)
    splitRemainder s = (s, LText.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> LText.Text -> LText.Text
    dropN = LText.drop . fromIntegral

    {-# INLINE dropExact #-}
    dropExact :: Int -> LText.Text -> Maybe LText.Text
    dropExact n xs =
        if LText.length xs < fromIntegral n
        then Nothing
        else Just $ LText.drop (fromIntegral n) xs

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> LText.Text -> LText.Text
    dropWith = LText.dropWhile

instance Eq a => Split a (Seq a) (Seq a) where
    {-# INLINE splitN #-}
    splitN :: Int -> Seq a -> (Seq a, Seq a)
    splitN = Seq.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    splitWith = Seq.spanl

    {-# INLINE splitExact #-}
    splitExact :: Int -> Seq a -> Maybe (Seq a, Seq a)
    splitExact n xs = if length xs < n then Nothing else Just (splitN n xs)

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

    {-# INLINE dropN #-}
    dropN :: Int -> Seq a -> Seq a
    dropN = Seq.drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> Seq a -> Maybe (Seq a)
    dropExact n xs =
        if length xs < n
        then Nothing
        else Just $ dropN n xs

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> Seq a -> Seq a
    dropWith = Seq.dropWhileL

instance Eq a => Split a (Vector.Vector a) (Vector.Vector a) where
    {-# INLINE splitN #-}
    splitN :: Int -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitN = Vector.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitWith = Vector.span

    {-# INLINE splitExact #-}
    splitExact :: Int -> Vector.Vector a -> Maybe (Vector.Vector a, Vector.Vector a)
    splitExact n xs = if length xs < n then Nothing else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Vector.Vector a -> Vector.Vector a -> Maybe (Vector.Vector a)
    matchPrefix xs ys =
        if length xs > length ys then Nothing else
            let (prefix, rest) = Vector.splitAt (length xs) ys in
                if xs == prefix then Just rest else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: Vector.Vector a -> (Vector.Vector a, Vector.Vector a)
    splitRemainder xs = (xs, Vector.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> Vector.Vector a -> Vector.Vector a
    dropN = Vector.drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> Vector.Vector a -> Maybe (Vector.Vector a)
    dropExact n xs =
        if length xs < n
        then Nothing
        else Just $ dropN n xs

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> Vector.Vector a -> Vector.Vector a
    dropWith = Vector.dropWhile

instance Eq a => Split a (SVector.Vector a) (SVector.Vector a) where
    {-# INLINE splitN #-}
    splitN :: Int -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitN = SVector.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitWith  = SVector.span

    {-# INLINE splitExact #-}
    splitExact :: Int -> SVector.Vector a -> Maybe (SVector.Vector a, SVector.Vector a)
    splitExact n xs = if length xs < n then Nothing else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: SVector.Vector a -> SVector.Vector a -> Maybe (SVector.Vector a)
    matchPrefix xs ys =
        if length xs > length ys then Nothing else
            let (prefix, rest) = SVector.splitAt (length xs) ys in
                if xs == prefix then Just rest else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: SVector.Vector a -> (SVector.Vector a, SVector.Vector a)
    splitRemainder xs = (xs, SVector.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> SVector.Vector a -> SVector.Vector a
    dropN = SVector.drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> SVector.Vector a -> Maybe (SVector.Vector a)
    dropExact n xs =
        if length xs < n
        then Nothing
        else Just $ dropN n xs

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> SVector.Vector a -> SVector.Vector a
    dropWith = SVector.dropWhile

instance (Eq a, UVector.Unbox a) => Split a (UVector.Vector a) (UVector.Vector a) where
    {-# INLINE splitN #-}
    splitN :: Int -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitN = UVector.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitWith = UVector.span

    {-# INLINE splitExact #-}
    splitExact :: Int -> UVector.Vector a -> Maybe (UVector.Vector a, UVector.Vector a)
    splitExact n xs = if UVector.length xs < n then Nothing else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: UVector.Vector a -> UVector.Vector a -> Maybe (UVector.Vector a)
    matchPrefix xs ys =
        if UVector.length xs > UVector.length ys then Nothing else
            let (prefix, rest) = UVector.splitAt (UVector.length xs) ys in
                if xs == prefix then Just rest else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: UVector.Vector a -> (UVector.Vector a, UVector.Vector a)
    splitRemainder xs = (xs, UVector.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> UVector.Vector a -> UVector.Vector a
    dropN = UVector.drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> UVector.Vector a -> Maybe (UVector.Vector a)
    dropExact n xs =
        if UVector.length xs < n
        then Nothing
        else Just $ dropN n xs

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> UVector.Vector a -> UVector.Vector a
    dropWith = UVector.dropWhile

instance (Eq a, StVector.Storable a) => Split a (StVector.Vector a) (StVector.Vector a) where
    {-# INLINE splitN #-}
    splitN :: Int -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitN = StVector.splitAt

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitWith = StVector.span

    {-# INLINE splitExact #-}
    splitExact :: Int -> StVector.Vector a -> Maybe (StVector.Vector a, StVector.Vector a)
    splitExact n xs = if StVector.length xs < n then Nothing else Just (splitN n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: StVector.Vector a -> StVector.Vector a -> Maybe (StVector.Vector a)
    matchPrefix xs ys =
        if StVector.length xs > StVector.length ys then Nothing else
            let (prefix, rest) = StVector.splitAt (StVector.length xs) ys in
                if xs == prefix then Just rest else Nothing

    {-# INLINE splitRemainder #-}
    splitRemainder :: StVector.Vector a -> (StVector.Vector a, StVector.Vector a)
    splitRemainder xs = (xs, StVector.empty)

    {-# INLINE dropN #-}
    dropN :: Int -> StVector.Vector a -> StVector.Vector a
    dropN = StVector.drop

    {-# INLINE dropExact #-}
    dropExact :: Int -> StVector.Vector a -> Maybe (StVector.Vector a)
    dropExact n xs =
        if StVector.length xs < n
        then Nothing
        else Just $ dropN n xs

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> StVector.Vector a -> StVector.Vector a
    dropWith = StVector.dropWhile
