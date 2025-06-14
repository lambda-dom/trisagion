{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib.Properties.Splittable (
    -- * Splittable properties.
    prop_mononaturality_single,
    prop_single_lists,

    -- * Property groups.
    splittableLaws,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word32)

-- Testing library.
import Hedgehog (PropertyT, Gen, Property, forAll, property)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor(..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Lib.Utils (withBinary)
import Trisagion.Typeclasses.Streamable (Streamable(..))
import Trisagion.Typeclasses.Splittable (Splittable (..))

-- Package testing.
import Lib.Generators (sizes)
import Lib.Predicate (predicates, fromPredicate)
import Lib.Function (Function, functions, fromFunction)
import Lib.Property (prop_function_extensional_equality)


{- | Mononaturality law for 'single'. -}
prop_mononaturality_single
    :: forall m s .
        (Monad m, Splittable s, MonoFunctor (PrefixOf s), ElementOf s ~ ElementOf (PrefixOf s),
        Eq (PrefixOf s), Show (PrefixOf s), Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> PropertyT m ()
prop_mononaturality_single elems = do
        f <- nat <$> forAll (functions elems elems)
        prop_function_extensional_equality
            (single @s . f)
            (monomap f . single @s)
            elems
    where
        nat :: Function (ElementOf s) (ElementOf s) -> ElementOf s -> ElementOf s
        nat = fromFunction (id :| []) ((.) :| (withBinary <$> [min, max]))

{- | Law for 'single' at the level of lists. -}
prop_single_lists
    :: forall m s .
        (Monad m, Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ ElementOf (PrefixOf s),
        Show (ElementOf s), Eq (ElementOf s))
    => Gen (ElementOf s)
    -> PropertyT m ()
prop_single_lists = prop_function_extensional_equality
        (: [])
        (monotoList . single @s)

{- | Mononaturality law for 'splitPrefix'. -}
prop_mononaturality_splitPrefix
    :: forall m s .
        (Monad m, Splittable s, Eq s, Show s, MonoFunctor (PrefixOf s), Eq (PrefixOf s),
        Show (PrefixOf s), ElementOf s ~ ElementOf (PrefixOf s), Ord (ElementOf s), Show (ElementOf s))
    => Word32
    -> Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_mononaturality_splitPrefix n elems streams = do
        i <- forAll $ sizes n
        f <- nat <$> forAll (functions elems elems)
        prop_function_extensional_equality
            (splitPrefix i . monomap f)
            (bimap (monomap f) (monomap f) . splitPrefix i)
            streams
    where
        nat :: Function (ElementOf s) (ElementOf s) -> ElementOf s -> ElementOf s
        nat = fromFunction (id :| []) ((.) :| (withBinary <$> [min, max]))

{- | Law for 'splitPrefix' at the level of lists. -}
prop_splitPrefix_lists
    :: (Monad m, Splittable s, Show s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ ElementOf s,
    Eq (ElementOf s), Show (ElementOf s))
    => Word32
    -> Gen s
    -> PropertyT m ()
prop_splitPrefix_lists n streams = do
    i <- forAll $ sizes n
    prop_function_extensional_equality
        (bimap monotoList toList . splitPrefix i)
        (splitAt (fromIntegral i) . toList)
        streams

{- | Compatibility law for 'uncons' and 'splitPrefix'. -}
prop_compatibility_uncons_splitPrefix
    :: (Monad m, Splittable s, Show s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ ElementOf s,
    Eq (ElementOf s), Show (ElementOf s))
    => Gen s
    -> PropertyT m ()
prop_compatibility_uncons_splitPrefix = prop_function_extensional_equality
    (maybe ([], []) (bimap (: []) toList) . uncons)
    (bimap monotoList toList . splitPrefix 1)

{- | Law for 'splitPrefix' at the level of lists. -}
prop_splitWith_lists
    :: forall m s .
        (Monad m, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ ElementOf s,
        Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_splitWith_lists elems streams = do
    p <- fromPredicate <$> forAll (predicates elems)
    prop_function_extensional_equality
        (bimap monotoList toList . splitWith p)
        (span p . toList)
        streams


{- | 'Splittable' laws property group. -}
splittableLaws
    :: forall s . (Splittable s, MonoFoldable (PrefixOf s), Eq s, Show s, ElementOf (PrefixOf s) ~ ElementOf s,
        Eq (PrefixOf s), Show (PrefixOf s), Ord (ElementOf s), Show (ElementOf s))
    => Word32
    -> Gen (ElementOf s)
    -> Gen s
    -> (String, [(String, Property)])
splittableLaws n elems streams = ("Splittable laws", fmap property <$> props)
    where
        props = [
            -- ("Mononaturality of single", property $ prop_mononaturality_single elems),
            -- ("single at the level of lists", prop_single_lists elems),
            ("Mononaturality for splitPrefix", prop_mononaturality_splitPrefix n elems streams),
            ("splitPrefix at the level of lists", prop_splitPrefix_lists n streams),
            ("Compatibility between splitPrefix and uncons", prop_compatibility_uncons_splitPrefix streams),
            ("splitWith at the level of lists", prop_splitWith_lists elems streams)
            ]
