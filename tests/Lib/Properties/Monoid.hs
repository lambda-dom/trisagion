module Lib.Properties.Monoid (
    -- * Properties.
    -- ** Monoid properties.
    prop_monoid_commutativity,
    prop_monoid_idempotency,

    -- * Property groups.
    monoidLaws,
    monoidMorphismLaws,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Tuple (swap)

-- Testing library.
import Hedgehog (PropertyT, Gen, (===), Property, property)

-- Package.
import Trisagion.Lib.Utils (diagonal)

-- Package testing.
import Lib.Property (prop_function_extensional_equality)


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
            triples
    where
        triples = do
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
        ((,) <$> gen <*> gen)

{- | Monoid idempotency. -}
prop_monoid_idempotency :: (Monad m, Eq a, Show a, Monoid a) => Gen a -> PropertyT m ()
prop_monoid_idempotency = prop_function_extensional_equality (uncurry (<>) . diagonal) id

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
        ((,) <$> gen <*> gen)


{- | Property group for the 'Monoid' laws. -}
monoidLaws
    :: (Monoid a, Eq a, Show a)
    => Gen a
    -> (String, [(String, Property)])
monoidLaws elems = ("Monoid laws", fmap property <$> props)
    where
        props = [
            ("Monoid left identity", prop_monoid_left_identity elems),
            ("Monoid right identity", prop_monoid_right_identity elems),
            ("Monoid associativity", prop_monoid_associativity elems)
            ]

{- | Property group for the 'Monoid' morphism laws. -}
monoidMorphismLaws
    :: (Monoid a, Show a, Monoid b, Eq b, Show b)
    => (a -> b)
    -> Gen a
    -> (String, [(String, Property)])
monoidMorphismLaws f elems = ("Monoid morphism laws", fmap property <$> props)
    where
        props = [
            ("Monoid morphism unit", prop_monoid_morphism_unit f),
            ("Monoid morphism composition", prop_monoid_morphism_mult f elems)
            ]
