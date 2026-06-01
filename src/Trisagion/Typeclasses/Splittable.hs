{- |
Module: Trisagion.Typeclasses.Splittable

The 'Streamable' typeclass allows us to, in principle, write all the needed parsers, but the
implementations can be very inefficient. To address that, the 'Splittable' typeclass adds new
primitives.
-}

module Trisagion.Typeclasses.Splittable (
    -- * Typeclasses.
    Splittable (..),
) where

-- Imports.
-- Base.
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, span, splitAt, empty, drop, dropWhile, singleton, length, stripPrefix)
import qualified Data.ByteString.Lazy as LBytes (ByteString, span, splitAt, empty, drop, dropWhile, singleton, length, stripPrefix)
import qualified Data.ByteString.Short as SBytes (ShortByteString, span, splitAt, empty, drop, dropWhile, singleton, length, stripPrefix)
import qualified Data.Text as Text (Text, span, splitAt, empty, drop, dropWhile, singleton, stripPrefix, length)
import qualified Data.Text.Lazy as LText (Text, span, splitAt, empty, drop, dropWhile, singleton, stripPrefix, length)
import Data.Sequence (Seq, ViewL (..), empty, viewl, (<|))
import qualified Data.Sequence as Seq (spanl, splitAt, drop, dropWhileL, singleton)

-- Package.
import qualified Trisagion.Utils.List as List (splitAtExact, matchPrefix)
import Trisagion.Typeclasses.Streamable (Streamable)


{- | The @Splittable@ typeclass.

Mirroring the laws for the 'Streamable' typeclass, the first law is:

__Mononaturality__: With the constraints @('Mono.Typeclasses.MonoFunctor' a s,
'Mono.Typeclasses.MonoFunctor' a b)@, @singleton@ and @splitPrefix n@ are mononatural.

__Singleton singleton__: The second law says that @singleton@ is @'Data.List.singleton'@ at the
level of lists:

prop> singleton == toList . singleton

__List identities__: For the third law, assuming a
@'Mono.Typeclasses.MonoFoldable.MonoFoldable' a b@ constraint on the prefix, then at the level of
lists @splitPrefix@ is 'Data.List.splitAt' and @splitWith@ is 'Data.List.span':

prop> bimap monotoList toList . splitPrefix n == splitAt n . toList
prop> bimap monotoList toList . splitWith p == span p . toList

__Compatibility__: The fourth and final law is a compatibility condition between
'Trisagion.Typeclasses.Streamable.uncons' and @splitPrefix@:

prop> maybe ([], []) (bimap singleton toList) . uncons == bimap monotoList toList . splitPrefix 1

=== __Counterexample:__

The following example shows that @'splitWith' p@ is /not/ mononatural.

>>> let p = ('\NUL' ==)
>>> let f = const '\NUL'
>>> splitWith p . monomap f $ "a"
("\NUL","")
>>> bimap (monomap f) (monomap f) . splitWith p $ "a"
("","\NUL")
-}
class Streamable a s => Splittable a b s | s -> b where
    {-# MINIMAL splitPrefix, splitWith, singleton, splitPrefixExact, matchPrefix #-}

    {- | Split the stream at offset @n@ into a pair @(prefix, suffix)@. -}
    splitPrefix :: Word -> s -> (b, s)

    {- | Split the longest prefix from the stream whose elements satisfy a predicate. -}
    splitWith :: (a -> Bool) -> s -> (b, s)

    {- | Convert a stream element to a prefix. -}
    singleton :: forall t -> s ~ t => a -> b

    {- | Split a prefix of exact size. -}
    splitPrefixExact :: Word -> s -> Maybe (b, s)

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
    dropPrefix :: Word -> s -> s
    dropPrefix n = snd . splitPrefix n

    {- | Drop the longest prefix satisfying a predicate from the stream.

    Default implementation is:

    @dropWith = snd . splitWith p@
    -}
    dropWith :: (a -> Bool) -> s -> s
    dropWith p = snd . splitWith p


-- Instances.
-- Base.
instance Eq a => Splittable a [a] [a] where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> [a] -> ([a], [a])
    splitPrefix n = splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith p = span p

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> [a] -> Maybe ([a], [a])
    splitPrefixExact n = List.splitAtExact n

    {-# INLINE matchPrefix #-}
    matchPrefix :: [a] -> [a] -> Maybe [a]
    matchPrefix xs = List.matchPrefix xs

    {-# INLINE singleton #-}
    singleton _ x = [x]

    {-# INLINE splitRemainder #-}
    splitRemainder :: [a] -> ([a], [a])
    splitRemainder xs = (xs, [])

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> [a] -> [a]
    dropPrefix n = drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> [a] -> [a]
    dropWith = dropWhile


-- Libraries.
instance Splittable Word8 Bytes.ByteString Bytes.ByteString where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitPrefix n = Bytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitWith = Bytes.span

    {-# INLINE singleton #-}
    singleton Bytes.ByteString = Bytes.singleton

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> Bytes.ByteString -> Maybe (Bytes.ByteString, Bytes.ByteString)
    splitPrefixExact n xs = if Bytes.length xs < fromIntegral n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Bytes.ByteString -> Bytes.ByteString -> Maybe Bytes.ByteString
    matchPrefix xs ys = Bytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitRemainder s = (s, Bytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> Bytes.ByteString -> Bytes.ByteString
    dropPrefix n = Bytes.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> Bytes.ByteString -> Bytes.ByteString
    dropWith = Bytes.dropWhile

instance Splittable Word8 LBytes.ByteString LBytes.ByteString where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitPrefix n = LBytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitWith = LBytes.span

    {-# INLINE singleton #-}
    singleton LBytes.ByteString = LBytes.singleton

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> LBytes.ByteString -> Maybe (LBytes.ByteString, LBytes.ByteString)
    splitPrefixExact n xs = if LBytes.length xs < fromIntegral n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: LBytes.ByteString -> LBytes.ByteString -> Maybe LBytes.ByteString
    matchPrefix xs ys = LBytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: LBytes.ByteString -> (LBytes.ByteString, LBytes.ByteString)
    splitRemainder s = (s, LBytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> LBytes.ByteString -> LBytes.ByteString
    dropPrefix n = LBytes.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> LBytes.ByteString -> LBytes.ByteString
    dropWith = LBytes.dropWhile

instance Splittable Word8 SBytes.ShortByteString SBytes.ShortByteString where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitPrefix n = SBytes.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitWith = SBytes.span

    {-# INLINE singleton #-}
    singleton SBytes.ShortByteString = SBytes.singleton

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> SBytes.ShortByteString -> Maybe (SBytes.ShortByteString, SBytes.ShortByteString)
    splitPrefixExact n xs = if SBytes.length xs < fromIntegral n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: SBytes.ShortByteString -> SBytes.ShortByteString -> Maybe SBytes.ShortByteString
    matchPrefix xs ys = SBytes.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: SBytes.ShortByteString -> (SBytes.ShortByteString, SBytes.ShortByteString)
    splitRemainder s = (s, SBytes.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> SBytes.ShortByteString -> SBytes.ShortByteString
    dropPrefix n = SBytes.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Word8 -> Bool) -> SBytes.ShortByteString -> SBytes.ShortByteString
    dropWith = SBytes.dropWhile

instance Splittable Char Text.Text Text.Text where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Text.Text -> (Text.Text, Text.Text)
    splitPrefix n = Text.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

    {-# INLINE singleton #-}
    singleton Text.Text = Text.singleton

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> Text.Text -> Maybe (Text.Text, Text.Text)
    splitPrefixExact n xs = if Text.length xs < fromIntegral n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Text.Text -> Text.Text -> Maybe Text.Text
    matchPrefix xs ys = Text.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: Text.Text -> (Text.Text, Text.Text)
    splitRemainder s = (s, Text.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> Text.Text -> Text.Text
    dropPrefix n = Text.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> Text.Text -> Text.Text
    dropWith = Text.dropWhile

instance Splittable Char LText.Text LText.Text where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> LText.Text -> (LText.Text, LText.Text)
    splitPrefix n = LText.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> LText.Text -> (LText.Text, LText.Text)
    splitWith = LText.span

    {-# INLINE singleton #-}
    singleton LText.Text = LText.singleton

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> LText.Text -> Maybe (LText.Text, LText.Text)
    splitPrefixExact n xs = if LText.length xs < fromIntegral n then Nothing else Just (splitPrefix n xs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: LText.Text -> LText.Text -> Maybe LText.Text
    matchPrefix xs ys = LText.stripPrefix xs ys

    {-# INLINE splitRemainder #-}
    splitRemainder :: LText.Text -> (LText.Text, LText.Text)
    splitRemainder s = (s, LText.empty)

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> LText.Text -> LText.Text
    dropPrefix n = LText.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> LText.Text -> LText.Text
    dropWith = LText.dropWhile

instance Eq a => Splittable a (Seq a) (Seq a) where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> Seq a -> (Seq a, Seq a)
    splitPrefix n = Seq.splitAt $ fromIntegral n

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    splitWith = Seq.spanl

    {-# INLINE singleton #-}
    singleton (Seq _) = Seq.singleton

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> Seq a -> Maybe (Seq a, Seq a)
    splitPrefixExact n xs = if length xs < fromIntegral n then Nothing else Just (splitPrefix n xs)

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
    dropPrefix :: Word -> Seq a -> Seq a
    dropPrefix n = Seq.drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> Seq a -> Seq a
    dropWith = Seq.dropWhileL

