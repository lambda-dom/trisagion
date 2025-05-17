{- |
Module: Trisagion.Lib.Utils

Misc untilities that have found no better place.
-}

module Trisagion.Lib.Utils (
    -- * List functions.
    enumDown,
    enumUp,
) where

{- | Enumerate the elements of a list downwards from @n@ to @0@.

The resulting list has at most @n + 1@ elements.
-}
{-# INLINE enumDown #-}
enumDown :: Word -> [a] -> [(Word, a)]
enumDown n = zip ns
    where
        ns = if n == 0 then [0] else [n, pred n .. 0]

{- | Enumerate the elements of a list upwards from @n@. -}
{-# INLINE enumUp #-}
enumUp :: Word -> [a] -> [(Word, a)]
enumUp n = zip [n ..]
