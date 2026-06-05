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
enumDown :: Int -> [a] -> [(Int, a)]
enumDown n = if n < 0 then const [] else zip ns
    where
        ns = if n == 0 then [0] else [n, pred n .. 0]

{- | Exact version of 'splitAt'.

=== __Examples:__

>>> splitAtExact 3 "01234"
Just ("012","34")

>>> splitAtExact 0 "01234"
Just ("","01234")

>>> splitAtExact 10 "01234"
Nothing
-}
{-# INLINEABLE splitAtExact #-}
splitAtExact :: Int -> [a] -> Maybe ([a], [a])
splitAtExact n xs
    | n <= 0    = Just ([], xs)
    | otherwise = case xs of
        []       -> Nothing
        (y : ys) -> fmap (first (y :)) $ splitAtExact (pred n) ys

{- | Match a prefix list, returning the tail.

=== __Examples:__

>>> matchPrefix "012" "01234"
Just "34"

>>> matchPrefix "012" "01"
Nothing
-}
{-# INLINEABLE matchPrefix #-}
matchPrefix :: Eq b => [b] -> [b] -> Maybe [b]
matchPrefix []       x        = Just x
matchPrefix (_ : _)  []       = Nothing
matchPrefix (y : ys) (z : zs) = if y == z then matchPrefix ys zs else Nothing
