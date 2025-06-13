{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib.Properties.Splittable (
    -- * Splittable properties.
    prop_mononaturality_single,
    prop_single_lists,
    prop_mononaturality_split_prefix,
    prop_split_prefix_lists,
    prop_compatibility_uncons_split_prefix,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word32)

-- Testing library.
import Hedgehog (PropertyT, Gen, forAll)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor(..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Lib.Utils (withBinary)
import Trisagion.Typeclasses.Streamable (Streamable(..))
import Trisagion.Typeclasses.Splittable (Splittable (..))

-- Package testing.
import Lib.Generators (sizes)
import Lib.Function (Function, functions, fromFunction)
import Lib.Property (prop_function_extensional_equality)


{- | Mononaturality law for 'single'. -}
prop_mononaturality_single
    :: forall m s .
        (Monad m, Splittable s, MonoFunctor (PrefixOf s), ElementOf s ~ ElementOf (PrefixOf s),
        Eq (PrefixOf s), Show (PrefixOf s), Ord (ElementOf (PrefixOf s)), Show (ElementOf (PrefixOf s)))
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
prop_mononaturality_split_prefix
    :: forall m s .
        (Monad m, Splittable s, Eq s, Show s, MonoFunctor (PrefixOf s), Eq (PrefixOf s),
        Show (PrefixOf s), ElementOf s ~ ElementOf (PrefixOf s), Ord (ElementOf s), Show (ElementOf s))
    => Word32
    -> Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_mononaturality_split_prefix n elems streams = do
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
prop_split_prefix_lists
    :: (Monad m, Splittable s, Show s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ ElementOf s,
    Eq (ElementOf s), Show (ElementOf s))
    => Word32
    -> Gen s
    -> PropertyT m ()
prop_split_prefix_lists n streams = do
    i <- forAll $ sizes n
    prop_function_extensional_equality
        (bimap monotoList toList . splitPrefix i)
        (splitPrefix i . toList)
        streams

{- | Compatibility law for 'uncons' and 'splitPrefix'. -}
prop_compatibility_uncons_split_prefix
    :: (Monad m, Splittable s, Show s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ ElementOf s,
    Eq (ElementOf s), Show (ElementOf s))
    => Gen s
    -> PropertyT m ()
prop_compatibility_uncons_split_prefix = prop_function_extensional_equality
    (maybe ([], []) (bimap (: []) toList) . uncons)
    (bimap monotoList toList . splitPrefix 1)
