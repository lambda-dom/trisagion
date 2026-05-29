{- |
Module: Trisagion.Typeclasses.Splittable

The @Splittable@ typeclass.
-}

module Trisagion.Typeclasses.Splittable (
    -- * Typeclasses.
    Splittable (..),
) where

-- Imports.
-- Package.
import qualified Trisagion.Utils.List as List (splitAtExact, matchPrefix)
import Trisagion.Typeclasses.Streamable (Streamable)


{- | The @Splittable@ typeclass.

Mirroring the laws for the 'Streamable' typeclass, the first law is:

__Mononaturality__: With the constraints @('Mono.Typeclasses.MonoFunctor' a s,
'Mono.Typeclasses.MonoFunctor' a b)@, @singleton@ and @splitPrefix n@ are mononatural.

__Singleton singleton__: The second law says that @singleton@ is @'Data.List.singleton'@ at the
level of lists:

prop> singleton == toList . singleton

__List identities__: For the third law, assuming a
@'Mono.Typeclasses.MonoFoldable.MonoFoldable' a b@ constraint on the prefix, then at the level of
lists @splitPrefix@ is 'Data.List.splitAt' and @splitWith@ is 'Data.List.span':

prop> bimap monotoList toList . splitPrefix n == splitAt n . toList
prop> bimap monotoList toList . splitWith p == span p . toList

__Compatibility__: The fourth and final law is a compatibility condition between
'Trisagion.Typeclasses.Streamable.uncons' and @splitPrefix@:

prop> maybe ([], []) (bimap singleton toList) . uncons == bimap monotoList toList . splitPrefix 1

=== __Counterexample:__

The following example shows that @'splitWith' p@ is /not/ mononatural.

>>> let p = ('\NUL' ==)
>>> let f = const '\NUL'
>>> splitWith p . monomap f $ "a"
("\NUL","")
>>> bimap (monomap f) (monomap f) . splitWith p $ "a"
("","\NUL")
-}
class Streamable a s => Splittable a b s | s -> b where
    {-# MINIMAL splitPrefix, splitWith, singleton, splitPrefixExact, matchPrefix #-}

    {- | Split the stream at offset @n@ into a pair @(prefix, suffix)@. -}
    splitPrefix :: Word -> s -> (b, s)

    {- | Split the longest prefix from the stream whose elements satisfy a predicate. -}
    splitWith :: (a -> Bool) -> s -> (b, s)

    {- | Convert a stream element to a prefix. -}
    singleton :: forall t -> s ~ t => a -> b

    {- | Split a prefix of exact size. -}
    splitPrefixExact :: Word -> s -> Maybe (b, s)

    {- | Parse and drop a matching prefix. -}
    matchPrefix :: b -> s -> Maybe s

    {- | Return the remainder of the stream as a prefix.

    Default implementation is:

    @splitRemainder = splitWith (const True)@

    Instances can (almost) always define a faster implementation using a nullary operation
    @empty :: s@.
    -}
    splitRemainder :: s -> (b, s)
    splitRemainder = splitWith (const True)

    {- | Drop a fixed-size prefix from the stream.

    Default implementation is:

    @dropPrefix n = snd . splitPrefix n@.
    -}
    dropPrefix :: Word -> s -> s
    dropPrefix n = snd . splitPrefix n

    {- | Drop the longest prefix satisfying a predicate from the stream.

    Default implementation is:

    @dropWith = snd . splitWith p@.
    -}
    dropWith :: (a -> Bool) -> s -> s
    dropWith p = snd . splitWith p


-- Instances.
instance Eq a => Splittable a [a] [a] where
    {-# INLINE splitPrefix #-}
    splitPrefix :: Word -> [a] -> ([a], [a])
    splitPrefix n = splitAt (fromIntegral n)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> [a] -> ([a], [a])
    splitWith p = span p

    {-# INLINE splitPrefixExact #-}
    splitPrefixExact :: Word -> [a] -> Maybe ([a], [a])
    splitPrefixExact n = List.splitAtExact n

    {-# INLINE matchPrefix #-}
    matchPrefix :: [a] -> [a] -> Maybe [a]
    matchPrefix xs = List.matchPrefix xs

    {-# INLINE singleton #-}
    singleton _ x = [x]

    {-# INLINE splitRemainder #-}
    splitRemainder :: [a] -> ([a], [a])
    splitRemainder xs = (xs, [])

    {-# INLINE dropPrefix #-}
    dropPrefix :: Word -> [a] -> [a]
    dropPrefix n = drop (fromIntegral n)

    {-# INLINE dropWith #-}
    dropWith :: (a -> Bool) -> [a] -> [a]
    dropWith = dropWhile
