{- |
Module: Trisagion.OStream

The @OStream@ typeclass for output streams, the return type of serializing functions.
-}

module Trisagion.OStream (
    -- * Typeclasses.
    OStream (..),
) where

-- Imports.
-- Base.
import GHC.IsList (fromList)
import Data.Foldable (Foldable (..))
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString.Builder as Bytes (Builder, word8)
import qualified Data.Text.Lazy.Builder as Text (Builder, singleton, fromString)
import Data.Sequence (Seq)
import qualified Data.Sequence as Sequence (singleton, fromList)


-- $setup
-- >>> import Data.Sequence


{- | The @OStream@ typeclass for output streams of elements @a@. -}
class Monoid s => OStream a s | s -> a where
    {-# MINIMAL one #-}

    {- | Construct an output stream from a single element. -}
    one :: a -> s

    {- | Construct an output stream from a list of elements.

    Default implementation is:

    @many = foldMap' single@
    -}
    many :: [a] -> s
    many = foldMap' one


-- Instances.
instance OStream a (Seq a) where
    {-# INLINE one #-}
    one :: a -> Seq a
    one = Sequence.singleton

    {-# INLINE many #-}
    many :: [a] -> Seq a
    many = Sequence.fromList

instance OStream Word8 Bytes.Builder where
    {-# INLINE one #-}
    one :: Word8 -> Bytes.Builder
    one n = Bytes.word8 n

    {-# INLINE many #-}
    many :: [Word8] -> Bytes.Builder
    many = fromList

instance OStream Char Text.Builder where
    {-# INLINE one #-}
    one :: Char -> Text.Builder
    one c = Text.singleton c

    {- | Performs replacement on invalid scalar values. -}
    {-# INLINE many #-}
    many :: [Char] -> Text.Builder
    many xs = Text.fromString xs
