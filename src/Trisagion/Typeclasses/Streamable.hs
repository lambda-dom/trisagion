{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),

    -- * Basic functions.
    toList,
    isSuffix,
) where


-- Imports.
-- Base.
import Data.List (unfoldr, isSuffixOf)

-- Libraries.
import Data.MonoFunctor (MonoFunctor (..))


{- | The @Streamable@ typeclass of monomorphic, streamable functors.

To describe the laws for the 'Streamable' typeclass, we start with a definition.

__Definition__: Let @s@ and @t@ be two monofunctors with @a ~ 'ElementOf' s ~ 'ElementOf' t@. A
function @h :: s -> t@ is /mononatural/ if for every @f :: a -> a@ we have the equality

@
monomap f . h = h . monomap f
@

Since @s@ is not polymorphic we do not have free theorems to rely on, so naturality must be
explicitly required:

__Naturality__: The function @'getOne' :: s -> 'Maybe' ('ElementOf' s, s)@ is natural.

In case it is not clear, the 'MonoFunctor' instance for @'Maybe' ('ElementOf' s, s)@ is:

@
monomap :: ('ElementOf' s -> 'ElementOf' s) -> 'Maybe' ('ElementOf' s, s) -> 'Maybe' ('ElementOf' s, s)
monomap f = fmap (bimap f (monomap f))
@
 
Given @'getOne' :: s -> 'Maybe' ('ElementOf' s, s)@ we can define @'toList' ::s -> [ElementOf s]@
by @'Data.List.unfoldr' 'getOne'@.

__Foldability__:

prop> 'MonoFoldable' s => monotoList = toList

Finally, the third law says that 'getOne' really is uncons-ing at the level of lists.

__Unconsing__:

prop> toList = maybe [] (\ (x, xs) -> x : toList xs) . getOne
-}
class MonoFunctor s => Streamable s where
    {-# MINIMAL getOne #-}

    {- | Get, or uncons, the first element from the streamable. -}
    getOne :: s -> Maybe (ElementOf s, s)


{- | Convert a 'Streamable' to a list. -}
toList :: Streamable s => s -> [ElementOf s]
toList = unfoldr getOne

{- | Return 'True' if @xs@ is a suffix of @ys@. -}
isSuffix :: (Streamable s, Eq (ElementOf s)) => s -> s -> Bool
isSuffix xs ys = toList xs `isSuffixOf` toList ys


-- Instances.
