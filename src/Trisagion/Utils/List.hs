{- |
Module: Trisagion.Utils.List

Some list utilities.
-}

module Trisagion.Utils.List (
    -- * Enumerations.
    enumDown,

    -- * Functions.
    splitAtExact,
    matchPrefix,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))


{- | Enumerate the elements of a list downwards from @n@ to @0@.

note(s):

  * The resulting list has at most @n + 1@ elements.
-}
{-# INLINE enumDown #-}
enumDown :: Word -> [a] -> [(Word, a)]
enumDown n = zip ns
    where
        ns = if n == 0 then [0] else [n, pred n .. 0]

{- | Exact version of 'splitAt'. -}
{-# INLINEABLE splitAtExact #-}
splitAtExact :: Word -> [a] -> Maybe ([a], [a])
splitAtExact 0 xs       = Just ([], xs)
splitAtExact _ []       = Nothing
splitAtExact m (x : xs) = fmap (first (x :)) $ splitAtExact (pred m) xs

{- | Match a prefix list, returning the tail. -}
{-# INLINEABLE matchPrefix #-}
matchPrefix :: Eq b => [b] -> [b] -> Maybe [b]
matchPrefix []       x        = Just x
matchPrefix (_ : _)  []       = Nothing
matchPrefix (y : ys) (z : zs) = if y == z then matchPrefix ys zs else Nothing
