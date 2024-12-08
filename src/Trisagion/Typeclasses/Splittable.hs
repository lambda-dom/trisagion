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
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import Data.MonoTraversable (Element)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.ByteString as Bytes (ByteString, span, splitAt, empty)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, span, splitAt, empty)
import qualified Data.Text as Text (Text, span, splitAt, empty)
import qualified Data.Text.Lazy as LazyText (Text, span, splitAt, empty)
import qualified Data.Sequence as Seq (spanl, splitAt, empty)
import qualified Data.Vector as Vector (span, splitAt, empty)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))


{- | The @Splittable@ typeclass of monomorphic splittable functors. -}
class Streamable s => Splittable s where
    {-# MINIMAL getAt, getWith #-}

    {- | The type of prefixes of the streamable. -}
    type PrefixOf s :: Type

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@. -}
    getAt :: Word -> s -> (PrefixOf s, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.
    
    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    getWith :: (Element s -> Bool) -> s -> (PrefixOf s, s)

    {- | Get the remainder of the stream as a prefix. -}
    getRemainder :: s -> (PrefixOf s, s)
    getRemainder = getWith (const True)


-- Instances.
instance Splittable Bytes.ByteString where
    type PrefixOf Bytes.ByteString = Bytes.ByteString

    getAt :: Word -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    getAt n = Bytes.splitAt $ fromIntegral n

    getWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    getWith = Bytes.span

    getRemainder :: Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    getRemainder xs = (xs, Bytes.empty)

instance Splittable LazyBytes.ByteString where
    type PrefixOf LazyBytes.ByteString = LazyBytes.ByteString

    getAt :: Word -> LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    getAt n = LazyBytes.splitAt $ fromIntegral n

    getWith :: (Word8 -> Bool) -> LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    getWith = LazyBytes.span

    getRemainder :: LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    getRemainder xs = (xs, LazyBytes.empty)

instance Splittable Text.Text where
    type PrefixOf Text.Text = Text.Text

    getAt :: Word -> Text.Text -> (Text.Text, Text.Text)
    getAt n = Text.splitAt $ fromIntegral n

    getWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    getWith = Text.span

    getRemainder :: Text.Text -> (Text.Text, Text.Text)
    getRemainder xs = (xs, Text.empty)

instance Splittable LazyText.Text where
    type PrefixOf LazyText.Text = LazyText.Text

    getAt :: Word -> LazyText.Text -> (LazyText.Text, LazyText.Text)
    getAt n = LazyText.splitAt $ fromIntegral n

    getWith :: (Char -> Bool) -> LazyText.Text -> (LazyText.Text, LazyText.Text)
    getWith = LazyText.span

    getRemainder :: LazyText.Text -> (LazyText.Text, LazyText.Text)
    getRemainder xs = (xs, LazyText.empty)

instance Splittable [a] where
    type PrefixOf [a] = [a]

    getAt :: Word -> [a] -> ([a], [a])
    getAt n = splitAt $ fromIntegral n

    getWith :: (a -> Bool) -> [a] -> ([a], [a])
    getWith = span

    getRemainder :: [a] -> ([a], [a])
    getRemainder xs = (xs, [])

instance Splittable (Seq a) where
    type PrefixOf (Seq a) = Seq a

    getAt :: Word -> Seq a -> (Seq a, Seq a)
    getAt n = Seq.splitAt $ fromIntegral n

    getWith :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    getWith = Seq.spanl

    getRemainder :: Seq a -> (Seq a, Seq a)
    getRemainder xs = (xs, Seq.empty)

instance Splittable (Vector a) where
    type PrefixOf (Vector a) = Vector a

    getAt :: Word -> Vector a -> (Vector a, Vector a)
    getAt n = Vector.splitAt (fromIntegral n)

    getWith :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
    getWith = Vector.span

    getRemainder :: Vector a -> (Vector a, Vector a)
    getRemainder xs = (xs, Vector.empty)
