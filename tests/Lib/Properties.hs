module Lib.Properties (
    -- * Extensional functional equality.
    prop_function_extensional_equality,

    -- * Monoid properties.
    prop_monoid_left_identity,
    prop_monoid_right_identity,
    prop_monoid_associativity,
    prop_monoid_commutativity,
    prop_monoid_idempotency,

    -- * Monoid morphism properties.
    prop_monoid_morphism_unit,
    prop_monoid_morphism_mult,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.Tuple (swap)

-- Testing.
import Hedgehog (Gen, PropertyT, (===), forAll)


{- | Testing extensional equality of functions. -}
prop_function_extensional_equality
    :: (Monad m, Show a, Eq b, Show b)
    => (a -> b)
    -> (a -> b)
    -> Gen a
    -> PropertyT m ()
prop_function_extensional_equality f g gen = do
    x <- forAll gen
    f x === g x


{- | Monoid left identity. -}
prop_monoid_left_identity :: (Monad m, Eq a, Show a, Monoid a) => Gen a -> PropertyT m ()
prop_monoid_left_identity = prop_function_extensional_equality (mempty <>) id

{- | Monoid right identity. -}
prop_monoid_right_identity :: (Monad m, Eq a, Show a, Monoid a) => Gen a -> PropertyT m ()
prop_monoid_right_identity = prop_function_extensional_equality (<> mempty) id

{- | Monoid associativity. -}
prop_monoid_associativity :: (Monad m, Eq a, Show a, Monoid a) => Gen a -> PropertyT m ()
prop_monoid_associativity gen =
        prop_function_extensional_equality
            (\ (x, y, z) -> (x <> y) <> z)
            (\ (x, y, z) -> x <> (y <> z))
            genTriples
    where
        genTriples = do
            x <- gen
            y <- gen
            z <- gen
            pure (x, y, z)

{- | Monoid commutativity. -}
prop_monoid_commutativity :: (Monad m, Eq a, Show a, Monoid a) => Gen a -> PropertyT m ()
prop_monoid_commutativity gen =
        prop_function_extensional_equality
            (uncurry (<>) . swap)
            (uncurry (<>))
            genPairs
    where
        genPairs = (,) <$> gen <*> gen

{- | Monoid idempotency. -}
prop_monoid_idempotency :: (Monad m, Eq a, Show a, Monoid a) => Gen a -> PropertyT m ()
prop_monoid_idempotency = prop_function_extensional_equality (uncurry (<>) . dup) id
    where
        dup :: b -> (b, b)
        dup x = (x, x)

{- | Monoid morphisms preserve the unit. -}
prop_monoid_morphism_unit
    :: (Monad m, Monoid a, Monoid b, Eq b, Show b)
    => (a -> b)
    -> PropertyT m ()
prop_monoid_morphism_unit f = do
    f mempty === mempty

{- | Monoid morphisms preserve the monoid binary operation. -}
prop_monoid_morphism_mult
    :: (Monad m, Monoid a, Show a, Monoid b, Eq b, Show b)
    => (a -> b)
    -> Gen a
    -> PropertyT m ()
prop_monoid_morphism_mult f gen =
        prop_function_extensional_equality
            (uncurry (<>) . bimap f f)
            (f . uncurry (<>))
            genPairs
    where
        genPairs = (,) <$> gen <*> gen
