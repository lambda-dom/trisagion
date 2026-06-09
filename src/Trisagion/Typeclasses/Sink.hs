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
import Data.ByteString.Builder (Builder, word8, byteString)


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

instance Sink Word8 ByteString Builder where

    snoc :: Builder -> Word8 -> Builder
    snoc xs n = xs <> word8 n

    append :: Builder -> ByteString -> Builder
    append xs ys = xs <> byteString ys
