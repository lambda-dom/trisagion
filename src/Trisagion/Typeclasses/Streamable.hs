{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),

    -- * Functions.
    isSuffix,
) where

-- Imports.
-- Base.
import Data.List (unfoldr)
import qualified Data.List as List (uncons, null, isSuffixOf)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor)


{- | The @Streamable@ typeclass of input streams. -}
class MonoFunctor a s => Streamable a s | s -> a where
    {-# MINIMAL uncons #-}

    {- | Get the first element from the stream. -}
    uncons :: s -> Maybe (a, s)

    {- | Return 'True' if the stream has no more elements. -}
    null :: s -> Bool
    null = maybe True (const False) . uncons

    {- | Convert a 'Streamable' to a list.

    Default implementation is:

    @toList = unfoldr uncons@.
    -}
    toList :: s -> [a]
    toList = unfoldr uncons


-- Instances.
instance Streamable a (Maybe a) where
    {-# INLINE uncons #-}
    uncons :: Maybe a -> Maybe (a, Maybe a)
    uncons xs =
        case xs of
            Just x -> Just (x, Nothing)
            _      -> Nothing

    {-# INLINE null #-}
    null :: Maybe a -> Bool
    null = maybe (True) (const False)

instance Streamable a [a] where
    {-# INLINE uncons #-}
    uncons :: [a] -> Maybe (a, [a])
    uncons = List.uncons

    {-# INLINE null #-}
    null :: [a] -> Bool
    null = List.null


{- | Return 'True' if @xs@ is a suffix of @ys@. -}
{-# INLINE isSuffix #-}
isSuffix :: (Streamable a s, Eq a) => s -> s -> Bool
isSuffix xs ys = toList xs `List.isSuffixOf` toList ys
