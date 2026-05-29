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
import Data.List (unfoldr, singleton)
import qualified Data.List as List (uncons, null, isSuffixOf)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor)


{- | The @Streamable@ typeclass of input streams.

We assume that all overriden instance implementations are extensionally equal to the default ones.
To describe the laws for the 'Streamable' typeclass, we start with a definition.

__Definition__: Let @'MonoFunctor' a s@ and @'MonoFunctor' a t@ be two monofunctors. A function
@h :: s -> t@ is /mononatural/ if for every @f :: a -> a@ we have the equality

@
monomap f . h == h . monomap f
@

Since @s@ is not polymorphic we do not have free theorems to rely on, so mononaturality must be
explicitly required:

__Naturality__: The function @'uncons' :: s -> 'Maybe' (a, s)@ is mononatural.

In case it is not clear, the 'MonoFunctor' instance for @'Maybe' (a, s)@ is
@monomap f = fmap (bimap f (monomap f))@.
 
__Foldability__: With the definition

@
'toList' ::s -> [a]
'toList' = 'Data.List.unfoldr' 'uncons'
@

and the @'Mono.Typeclasses.MonoFoldable.MonoFoldable' a b s@ constraint we have the equality:

prop> monotoList == toList

__Unconsing__: Finally, the third law says that 'uncons' really is uncons-ing at the level of lists.

prop> toList == maybe [] (\ (x, xs) -> x : toList xs) . uncons
-}
class MonoFunctor a s => Streamable a s | s -> a where
    {-# MINIMAL uncons #-}

    {- | Get the first element from the stream. -}
    uncons :: s -> Maybe (a, s)

    {- | Return 'True' if the stream has no more elements. -}
    null :: s -> Bool
    null = maybe True (const False) . uncons

    {- | Drop one element from the stream.

    Default implementation is:

    @dropOne s = maybe s snd $ uncons s@
    -}
    dropOne :: s -> s
    dropOne s = maybe s snd $ uncons s

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

    {-# INLINE dropOne #-}
    dropOne :: Maybe a -> Maybe a
    dropOne _ = Nothing

    {-# INLINE toList #-}
    toList :: Maybe a -> [a]
    toList = maybe [] singleton

instance Streamable a [a] where
    {-# INLINE uncons #-}
    uncons :: [a] -> Maybe (a, [a])
    uncons = List.uncons

    {-# INLINE null #-}
    null :: [a] -> Bool
    null = List.null

    {-# INLINE dropOne #-}
    dropOne :: [a] -> [a]
    dropOne = drop 1

    {-# INLINE toList #-}
    toList :: [a] -> [a]
    toList = id


{- | Return 'True' if @xs@ is a suffix of @ys@. -}
{-# INLINE isSuffix #-}
isSuffix :: (Streamable a s, Eq a) => s -> s -> Bool
isSuffix xs ys = toList xs `List.isSuffixOf` toList ys
