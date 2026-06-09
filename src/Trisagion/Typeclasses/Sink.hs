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

-- Libraries.
import Data.Sequence (Seq, (|>), (><))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Bytes (Builder, word8, byteString)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder, singleton, fromText, fromString)


-- $setup
-- >>> import Data.Sequence


{- | The @Sink@ typeclass for output streams. -}
class Monoid s => Sink a b s | s -> b, s -> a where
    {-# MINIMAL snoc, append #-}

    {- | Append an element to the end of the output stream. -}
    snoc :: s -> a -> s

    {- | Append a suffix to the end of the output stream. -}
    append :: s -> b -> s

    {- | Concatenate a list of elements to the end of the output stream.

    === __Examples:__

    >>> concatenate (fromList "01") "23"
    fromList "0123"
    -}
    concatenate :: s -> [a] -> s
    concatenate xs ys = foldr (flip snoc) xs ys


-- Instances.
instance Sink a (Seq a) (Seq a) where
    snoc :: Seq a -> a -> Seq a
    snoc = (|>)

    append :: Seq a -> Seq a -> Seq a
    append = (><)

    concatenate :: Seq a -> [a] -> Seq a
    concatenate xs []       = xs
    concatenate xs (y : ys) = concatenate (xs |> y) ys

instance Sink Word8 ByteString Bytes.Builder where

    snoc :: Bytes.Builder -> Word8 -> Bytes.Builder
    snoc xs n = xs <> Bytes.word8 n

    append :: Bytes.Builder -> ByteString -> Bytes.Builder
    append xs ys = xs <> Bytes.byteString ys

instance Sink Char Text Text.Builder where

    snoc :: Text.Builder -> Char -> Text.Builder
    snoc xs c = xs <> Text.singleton c

    append :: Text.Builder -> Text -> Text.Builder
    append xs ys = xs <> Text.fromText ys

    {- | Performs replacement on invalid scalar values. -}
    concatenate :: Text.Builder -> [Char] -> Text.Builder
    concatenate xs ys = xs <> Text.fromString ys
