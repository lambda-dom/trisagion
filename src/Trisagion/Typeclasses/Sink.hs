{- |
Module: Trisagion.Typeclasses.Sink

The @Sink@ typeclass for output streams.
-}

module Trisagion.Typeclasses.Sink (
    -- * Typeclasses.
    Sink (..),
) where

-- Imports.
-- Libraries.
import Data.Sequence (Seq, (|>), (><))


{- | The @Sink@ typeclass for output streams. -}
class Monoid s => Sink a b s | s -> b, s -> a where
    {-# MINIMAL snoc, append #-}

    {- | Append an element to the end of the output stream. -}
    snoc :: s -> a -> s

    {- | Append a suffix to the end of the output stream. -}
    append :: s -> b -> s

    {- | Concatenate a list of elements to the end of the output stream. -}
    concat :: s -> [a] -> s
    concat xs ys = foldl' snoc xs ys


-- Instances.
instance Sink a (Seq a) (Seq a) where
    snoc :: Seq a -> a -> Seq a
    snoc = (|>)

    append :: Seq a -> Seq a -> Seq a
    append = (><)
