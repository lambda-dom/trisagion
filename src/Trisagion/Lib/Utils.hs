{- |
Module: Trisagion.Lib.Utils

Misc untilities that have found no better place.
-}

module Trisagion.Lib.Utils (
    -- * Function combinators.
    withBinary,

    -- * Tuple functions.
    diagonal,

    -- * List functions.
    enumDown,
    enumUp,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))


{- | The diagonal, or duplicate, function. -}
diagonal :: a -> (a, a)
diagonal x = (x, x)

{- | Function combinator using a binary function. -}
withBinary :: (b -> b -> b) -> (a -> b) -> (a -> b) -> (a -> b)
withBinary h f g = uncurry h . bimap f g . diagonal


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
