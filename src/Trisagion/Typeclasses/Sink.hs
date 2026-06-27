{- |
Module: Trisagion.Typeclasses.Sink

The @Sink@ typeclass for output streams.
-}

module Trisagion.Typeclasses.Sink (
    -- * Typeclasses.
    Sink (..),
) where

-- Imports.
-- Base.
import Data.Word (Word8)
-- For bytestring serialization.
import GHC.IsList (fromList)

-- Libraries.
import Data.Sequence (Seq)
import qualified Data.Sequence as Sequence (singleton, fromList)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Bytes (Builder, word8, byteString)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder, singleton, fromText, fromString)


-- $setup
-- >>> import Data.Sequence


{- | The @Sink@ typeclass for output streams. -}
class Monoid s => Sink a b s | s -> b, s -> a where
    {-# MINIMAL single, suffix #-}

    {- | Construct an output stream from a single element. -}
    single :: a -> s

    {- | Construct an output stream from a suffix. -}
    suffix :: b -> s

    {- | Construct an output stream from a list of streams. -}
    many :: [a] -> s
    many = foldMap single


-- Instances.
instance Sink a (Seq a) (Seq a) where
    {-# INLINE single #-}
    single :: a -> Seq a
    single = Sequence.singleton

    {-# INLINE suffix #-}
    suffix :: Seq a -> Seq a
    suffix = id

    {-# INLINE many #-}
    many :: [a] -> Seq a
    many = Sequence.fromList

instance Sink Word8 ByteString Bytes.Builder where
    {-# INLINE single #-}
    single :: Word8 -> Bytes.Builder
    single n = Bytes.word8 n

    {-# INLINE suffix #-}
    suffix :: ByteString -> Bytes.Builder
    suffix xs = Bytes.byteString xs

    {-# INLINE many #-}
    many :: [Word8] -> Bytes.Builder
    many = fromList

instance Sink Char Text Text.Builder where
    {-# INLINE single #-}
    single :: Char -> Text.Builder
    single c = Text.singleton c

    {-# INLINE suffix #-}
    suffix :: Text -> Text.Builder
    suffix ys = Text.fromText ys

    {- | Performs replacement on invalid scalar values. -}
    {-# INLINE many #-}
    many :: [Char] -> Text.Builder
    many xs = Text.fromString xs
