{- |
Module: Trisagion.Typeclasses.Splittable

The @Splittable@ typeclass.
-}

module Trisagion.Typeclasses.Splittable (
    -- * Typeclasses.
    Splittable (..),
) where

-- Imports.
-- Base.
import Data.Kind (Type)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))


{- | The @Splittable@ typeclass of monomorphic splittable functors.

Mirroring the laws for the 'Streamable' typeclass, the first law is:

__Naturality__: With @('MonoFunctor' ('PrefixOf' s), 'ElementOf' ('PrefixOf' s) ~ 'ElementOf' s)@,
for every @n@ and every @p@, both '@getAt' n@ and @'getWith' p@ are mononatural.

For the second law, assuming @MonoFoldable ('PrefixOf' s)@ besides the above @'MonoFunctor'@
constraint, then at the level of lists 'getAt' is 'Data.List.splitAt' and 'getWith' 'Data.List.span':

__List identities__:

prop> bimap monotoList monotoList . getAt n = splitAt n . monotoList
prop> bimap monotoList monotoList . getWith p = span p . monotoList

The third and final law is a compatibility condition between 'getOne' and 'getAt':

__Compatibility__:

prop> maybe [] (bimap singleton monotoList) . getOne = bimap monotoList monotoList . getAt 1
 -}
class Streamable s => Splittable s where
    {-# MINIMAL getAt, getWith #-}

    {- | The type of prefixes of the streamable. -}
    type PrefixOf s :: Type

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@. -}
    getAt :: Word -> s -> (PrefixOf s, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.
    
    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    getWith :: (ElementOf s -> Bool) -> s -> (PrefixOf s, s)
