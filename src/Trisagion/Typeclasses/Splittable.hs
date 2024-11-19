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
import Data.ByteString (ByteString)
import Data.MonoTraversable (Element)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.ByteString as Bytes (span, splitAt)
import qualified Data.Sequence as Sequence (spanl, splitAt)
import qualified Data.Text as Text (span, splitAt)
import qualified Data.Vector as Vector (span, splitAt)

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


-- Instances.
instance Splittable [a] where
    type PrefixOf [a] = [a]

    getAt :: Word -> [a] -> ([a], [a])
    getAt n = splitAt $ fromIntegral n

    getWith :: (a -> Bool) -> [a] -> ([a], [a])
    getWith = span

instance Splittable ByteString where
    type PrefixOf ByteString = ByteString

    getAt :: Word -> ByteString -> (ByteString, ByteString)
    getAt n = Bytes.splitAt $ fromIntegral n

    getWith :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
    getWith = Bytes.span

instance Splittable (Seq a) where
    type PrefixOf (Seq a) = Seq a

    getAt :: Word -> Seq a -> (Seq a, Seq a)
    getAt n = Sequence.splitAt $ fromIntegral n

    getWith :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    getWith = Sequence.spanl

instance Splittable Text where
    type PrefixOf Text = Text

    getAt :: Word -> Text -> (Text, Text)
    getAt n = Text.splitAt $ fromIntegral n

    getWith :: (Char -> Bool) -> Text -> (Text, Text)
    getWith = Text.span

instance Splittable (Vector a) where
    type PrefixOf (Vector a) = Vector a

    getAt :: Word -> Vector a -> (Vector a, Vector a)
    getAt n = Vector.splitAt (fromIntegral n)

    getWith :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
    getWith = Vector.span
