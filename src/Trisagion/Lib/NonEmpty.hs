{- |
Module: Trisagion.Lib.NonEmpty

Some 'NonEmpty' utility functions.
-}

module Trisagion.Lib.NonEmpty (
    -- * Functions.
    uncons,
    zipExact,
    zipWithExact,
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty (uncons)


{- | Uncons the first element of a 'NonEmpty'. -}
uncons :: NonEmpty a -> Maybe (a, NonEmpty a)
uncons xs =
    case NonEmpty.uncons xs of
        (_, Nothing) -> Nothing
        (y, Just ys) -> Just (y, ys)

{- | Zip exactly two 'NonEmpty'. -}
zipExact :: NonEmpty a -> NonEmpty b -> Maybe (NonEmpty (a, b))
zipExact = zipWithExact (,)

{- | Zip exactly two 'NonEmpty' with a binary function. -}
zipWithExact :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> Maybe (NonEmpty c)
zipWithExact f (x :| xs) (y :| ys) = (f x y :|) <$> zipLists xs ys
    where
        zipLists = go
            where
                go []       []       = Just []
                go []       _        = Nothing
                go _        []       = Nothing
                go (x' : xs') (y' : ys') = (f x' y' :) <$> go xs' ys'
