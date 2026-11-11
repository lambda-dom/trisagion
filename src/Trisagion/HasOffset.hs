{- |
Module: Trisagion.HasOffset

The @HasOffset@ typeclass.
-}

module Trisagion.HasOffset (
    -- * Typeclasses.
    HasOffset (..),
) where


{- | The typeclass for input streams with a notion of current offset. -}
class HasOffset s where
    {-# MINIMAL offset #-}

    {- | Return the current stream offset. -}
    offset :: s -> Int
