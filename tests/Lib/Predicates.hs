module Lib.Predicates (
    -- * Types.
    Predicate,

    -- ** Functionalization.
    fromPredicate,

    -- ** Generators.
    genPredicate,
) where

-- Imports.
-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Internal.Gen as Gen (recursive, choice, bool, subterm, subterm2, filter)


{- | The 'Predicate' type. -}
data Predicate a where
    Const    :: Bool -> Predicate a
    Interval :: a -> a -> Predicate a
    Negate   :: Predicate a -> Predicate a
    And      :: Predicate a -> Predicate a -> Predicate a
    Or       :: Predicate a -> Predicate a -> Predicate a
    deriving stock (Eq, Show, Ord)


{- | Construct a predicate function from a @'Predicate' a@ value. -}
fromPredicate :: Ord a => Predicate a -> a -> Bool
fromPredicate r = case r of
    Const    b   -> const b
    Interval x y -> \ z -> x <= z && z <= y
    Negate   p   -> not . fromPredicate p
    And      p q -> \ x -> fromPredicate p x && fromPredicate q x
    Or       p q -> \ x -> fromPredicate p x || fromPredicate q x

{- | Generator for @'Predicate' a@ values. -}
genPredicate :: Ord a => Gen a -> Gen (Predicate a)
genPredicate gen = go
    where
        -- Guarantee intervals are non-empty.
        genIntervals = do
            x <- gen
            y <- Gen.filter (x <=) gen
            pure $ Interval x y

        go = Gen.recursive
            Gen.choice
            [Const <$> Gen.bool, genIntervals]
            [Gen.subterm go Negate, Gen.subterm2 go go And, Gen.subterm2 go go Or]
