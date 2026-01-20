{- |
Module: Trisagion.Utils.List

Some list utilities.
-}

module Trisagion.Utils.List (
    -- * Enumerations.
    enumDown,
) where


{- | Enumerate the elements of a list downwards from @n@ to @0@.

note(s):

  * The resulting list has at most @n + 1@ elements.
-}
{-# INLINE enumDown #-}
enumDown :: Word -> [a] -> [(Word, a)]
enumDown n = zip ns
    where
        ns = if n == 0 then [0] else [n, pred n .. 0]
