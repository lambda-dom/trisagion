{- |
Module: Trisagion.Typeclasses.HasOffset

The @HasOffset@ typeclass.
-}

module Trisagion.Typeclasses.HasOffset (
    -- * Typeclasses.
    HasOffset (..),
) where


{- | The typeclass for input streams with a notion of current offset. -}
class HasOffset s where
    {-# MINIMAL offset #-}

    {- | Getter for the current offset of the stream. -}
    offset :: s -> Word
