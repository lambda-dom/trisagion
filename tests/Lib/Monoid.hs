{-# LANGUAGE DataKinds #-}

module Lib.Monoid (
    -- * Predicates.
    isExtensionallyEqual,

    -- * Properties.
    -- ** Extensional equality of functions.
    test_extensional_equality,

    -- * Monoid typeclass laws.
    test_semigroup_law,
    test_monoid_laws,
    test_semigroup_idempotency,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Testing.
import Test.Falsify (Property, Gen, Predicate, (.$), assert, gen)
import Test.Falsify.Predicate (satisfies, eq)


{- | Predicate for functional, extensional equality of functions. -}
isExtensionallyEqual
    :: Eq b
    => (String, a -> b)
    -> (String, a -> b)
    -> Predicate '[a]
isExtensionallyEqual (xs, f) (ys, g) =
    satisfies (fromString $ xs ++ " == " ++ ys, \ x -> f x == g x)

{- | Test extensional equality of two functions. -}
test_extensional_equality
    :: (Show a, Eq b)
    => (String, a -> b)
    -> (String, a -> b)
    -> Gen a
    -> Property ()
test_extensional_equality p q genArg = do
        x <- gen genArg
        assert $ isExtensionallyEqual p q .$ (fromString "x", x)

{- | Test the semigroup law. -}
test_semigroup_law
    :: (Semigroup m, Eq m, Show m)
    => Gen m
    -> Property ()
test_semigroup_law g = do
    x <- gen g
    y <- gen g
    z <- gen g
    assert $ eq
        .$ (fromString "(x <> y) <> z", (x <> y) <> z)
        .$ (fromString "x <> (y <> z)", x <> (y <> z))

{- | Test the left identy law. -}
test_left_identity_monoid
    :: (Monoid m, Eq m, Show m)
    => Gen m
    -> Property ()
test_left_identity_monoid g =
    test_extensional_equality
        ("mempty <> id", (mempty <>))
        ("id", id)
        g

{- | Test the right identy law. -}
test_right_identity_monoid
    :: (Monoid m, Eq m, Show m)
    => Gen m
    -> Property ()
test_right_identity_monoid g =
    test_extensional_equality
        ("id <> mempty", (<> mempty))
        ("id", id)
        g

{- | Test the monoid laws, including associativity. -}
test_monoid_laws
    :: (Monoid m, Eq m, Show m)
    => Gen m
    -> Property ()
test_monoid_laws g
    =  test_semigroup_law g
    *> test_left_identity_monoid g
    *> test_right_identity_monoid g

{- | Test semigroup idempotency. -}
test_semigroup_idempotency
    :: (Semigroup m, Eq m, Show m)
    => Gen m
    -> Property ()
test_semigroup_idempotency g =
    test_extensional_equality
        (fromString "x <> x", uncurry (<>) . (\ x -> (x, x)))
        (fromString "id", id)
        g
