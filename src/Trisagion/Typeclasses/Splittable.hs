{- |
Module: Trisagion.Typeclasses.Splittable

The @Splittable@ typeclass.
-}

module Trisagion.Typeclasses.Splittable (
    -- * The 'Splittable' typeclass.
    --
    -- $splittable
    Splittable (..),
) where

-- Imports.
-- Base.
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.ByteString as Bytes (ByteString, span, splitAt, empty)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, span, splitAt, empty)
import qualified Data.Text as Text (Text, span, splitAt, empty)
import qualified Data.Text.Lazy as LazyText (Text, span, splitAt, empty)
import qualified Data.Sequence as Seq (spanl, splitAt, empty)
import qualified Data.Vector as Vector (span, splitAt, empty)

-- Package.
import Trisagion.Typeclasses.Streamable (ElementOf, Streamable (..))


-- $splittable
--
-- Mirroring the laws for the 'Streamable' typeclass, the first law is:
--
-- __Naturality__: both 'getAt' and 'getWith' are natural.
--
-- For the second law, let @(prefix, suffix)@ be @'getAt' n xs@ for arbitrary @n@ and @xs@. Since
-- @s@ is a @MonoFoldable@ both @xs@ and @suffix@ can be converted to lists. Given that, as per the
-- name, @suffix@ is supposed to be a suffix of @xs@ there is a unique list @l@ such that:
--
-- @
--   otoList xs = l ++ otoList suffix
-- @
--
-- It follows that @l@ is equal to:
--
-- @
--   l = take (olength xs - olength suffix) (otoList xs)
-- @
--
-- Therefore, assuming the constraints,
--
-- @
--   MonoFunctor (PrefixOf s), 'ElementOf' (PrefixOf s) ~ 'ElementOf' s, MonoFoldable (PrefixOf s)
-- @
--
-- which are satisfied by all instances of @'PrefixOf' s@ defined in the library as following from
-- the constraint @PrefixOf s ~ s@, the second typeclass law can be expressed as:
--
-- __Prefix__:
--
-- prop> otoList = uncurry (++) . bimap otoList otoList . getAt n
-- prop> otoList = uncurry (++) . bimap otoList otoList . getWith p
--
-- The third set of laws simply says that 'getAt' is 'take' on lists and 'getWith', 'takeWhile':
--
-- __Foldability__:
--
-- prop> otoList . getAt n = take n . otoList
-- prop> otoList . getWith p = takeWhile p . otoList
--
-- The fourth and final law is a compatibility condition between 'getOne' and 'getAt':
--
-- __Compatibility__:
--
-- prop> maybe [] singleton . getOne = otoList . getAt 1


{- | The @Splittable@ typeclass of monomorphic splittable functors. -}
class Streamable s => Splittable s where
    {-# MINIMAL getAt, getWith #-}

    {- | The type of prefixes of the streamable. -}
    type PrefixOf s :: Type

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@. -}
    getAt :: Word -> s -> (PrefixOf s, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.
    
    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    getWith :: (ElementOf s -> Bool) -> s -> (PrefixOf s, s)

    {- | Get the remainder of the stream as a prefix.

    By default, this is defined by @'getWith' ('const' 'True')@.

    note(s):

    * The existence of this function implies (but is stronger than) the existence of a conversion
    function @s -> 'PrefixOf' s@.
    -}
    getRemainder :: s -> (PrefixOf s, s)
    getRemainder = getWith (const True)


-- Instances.
instance Splittable Bytes.ByteString where
    type PrefixOf Bytes.ByteString = Bytes.ByteString

    {-# INLINE getAt #-}
    getAt :: Word -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    getAt n = Bytes.splitAt $ fromIntegral n

    {-# INLINE getWith #-}
    getWith :: (Word8 -> Bool) -> Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    getWith = Bytes.span

    {-# INLINE getRemainder #-}
    getRemainder :: Bytes.ByteString -> (Bytes.ByteString, Bytes.ByteString)
    getRemainder xs = (xs, Bytes.empty)

instance Splittable LazyBytes.ByteString where
    type PrefixOf LazyBytes.ByteString = LazyBytes.ByteString

    {-# INLINE getAt #-}
    getAt :: Word -> LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    getAt n = LazyBytes.splitAt $ fromIntegral n

    {-# INLINE getWith #-}
    getWith :: (Word8 -> Bool) -> LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    getWith = LazyBytes.span

    {-# INLINE getRemainder #-}
    getRemainder :: LazyBytes.ByteString -> (LazyBytes.ByteString, LazyBytes.ByteString)
    getRemainder xs = (xs, LazyBytes.empty)

instance Splittable Text.Text where
    type PrefixOf Text.Text = Text.Text

    {-# INLINE getAt #-}
    getAt :: Word -> Text.Text -> (Text.Text, Text.Text)
    getAt n = Text.splitAt $ fromIntegral n

    {-# INLINE getWith #-}
    getWith :: (Char -> Bool) -> Text.Text -> (Text.Text, Text.Text)
    getWith = Text.span

    {-# INLINE getRemainder #-}
    getRemainder :: Text.Text -> (Text.Text, Text.Text)
    getRemainder xs = (xs, Text.empty)

instance Splittable LazyText.Text where
    type PrefixOf LazyText.Text = LazyText.Text

    {-# INLINE getAt #-}
    getAt :: Word -> LazyText.Text -> (LazyText.Text, LazyText.Text)
    getAt n = LazyText.splitAt $ fromIntegral n

    {-# INLINE getWith #-}
    getWith :: (Char -> Bool) -> LazyText.Text -> (LazyText.Text, LazyText.Text)
    getWith = LazyText.span

    {-# INLINE getRemainder #-}
    getRemainder :: LazyText.Text -> (LazyText.Text, LazyText.Text)
    getRemainder xs = (xs, LazyText.empty)

instance Splittable [a] where
    type PrefixOf [a] = [a]

    {-# INLINE getAt #-}
    getAt :: Word -> [a] -> ([a], [a])
    getAt n = splitAt $ fromIntegral n

    {-# INLINE getWith #-}
    getWith :: (a -> Bool) -> [a] -> ([a], [a])
    getWith = span

    {-# INLINE getRemainder #-}
    getRemainder :: [a] -> ([a], [a])
    getRemainder xs = (xs, [])

instance Splittable (Seq a) where
    type PrefixOf (Seq a) = Seq a

    {-# INLINE getAt #-}
    getAt :: Word -> Seq a -> (Seq a, Seq a)
    getAt n = Seq.splitAt $ fromIntegral n

    {-# INLINE getWith #-}
    getWith :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    getWith = Seq.spanl

    {-# INLINE getRemainder #-}
    getRemainder :: Seq a -> (Seq a, Seq a)
    getRemainder xs = (xs, Seq.empty)

instance Splittable (Vector a) where
    type PrefixOf (Vector a) = Vector a

    {-# INLINE getAt #-}
    getAt :: Word -> Vector a -> (Vector a, Vector a)
    getAt n = Vector.splitAt (fromIntegral n)

    {-# INLINE getWith #-}
    getWith :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
    getWith = Vector.span

    {-# INLINE getRemainder #-}
    getRemainder :: Vector a -> (Vector a, Vector a)
    getRemainder xs = (xs, Vector.empty)
