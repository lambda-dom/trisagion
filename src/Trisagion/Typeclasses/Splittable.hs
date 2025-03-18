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
import qualified Data.List as List (splitAt)
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, span, splitAt)
import qualified Data.Text as Text (Text, span, splitAt)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (spanl, splitAt)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (span, splitAt)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))


{- | The @Splittable@ typeclass of monomorphic splittable functors.

Mirroring the laws for the 'Streamable' typeclass, the first law is:

__Naturality__: With @('MonoFunctor' ('PrefixOf' s), 'ElementOf' ('PrefixOf' s) ~ 'ElementOf' s)@,
for every @n@ and every @p@, both @splitAt n@ and @splitWith p@ are mononatural.

For the second law, assuming @MonoFoldable ('PrefixOf' s)@ besides the above @'MonoFunctor'@
constraint, then at the level of lists @splitAt@ is 'Data.List.splitAt' and @splitWith@ is
'Data.List.span':

__List identities__:

prop> bimap monotoList monotoList . splitAt n = splitAt n . monotoList
prop> bimap monotoList monotoList . splitWith p = span p . monotoList

The third and final law is a compatibility condition between 'splitOne' and @splitAt@:

__Compatibility__:

prop> maybe [] (bimap singleton monotoList) . splitOne = bimap monotoList monotoList . splitAt 1
 -}
class Streamable s => Splittable s where
    {-# MINIMAL splitAt, splitWith #-}

    {- | The type of prefixes of the streamable. -}
    type PrefixOf s :: Type

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@. -}
    splitAt :: Word -> s -> (PrefixOf s, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.
    
    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    splitWith :: (ElementOf s -> Bool) -> s -> (PrefixOf s, s)


-- Instances.
instance Splittable Bytes.ByteString where
    type PrefixOf Bytes.ByteString = Bytes.ByteString

    splitAt :: Word -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitAt n = Bytes.splitAt $ fromIntegral n

    splitWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    splitWith = Bytes.span

instance Splittable Text.Text where
    type PrefixOf Text.Text = Text.Text

    splitAt :: Word -> Text.Text -> (Text.Text, Text.Text)
    splitAt n = Text.splitAt $ fromIntegral n

    splitWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    splitWith = Text.span

instance Splittable [a] where
    type PrefixOf [a] = [a]

    splitAt :: Word -> [a] -> ([a], [a])
    splitAt n = List.splitAt $ fromIntegral n

    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith = span

instance Splittable (Seq a) where
    type PrefixOf (Seq a) = Seq a

    splitAt :: Word -> Seq a -> (Seq a, Seq a)
    splitAt n = Seq.splitAt $ fromIntegral n

    splitWith :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    splitWith = Seq.spanl

instance Splittable (Vector a) where
    type PrefixOf (Vector a) = Vector a

    splitAt :: Word -> Vector a -> (Vector a, Vector a)
    splitAt n = Vector.splitAt (fromIntegral n)

    splitWith :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
    splitWith = Vector.span
