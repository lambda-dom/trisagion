{-# LANGUAGE OverloadedStrings #-}

module Tests.Typeclasses.Streamable (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Hedgehog (PropertyT, Gen, Group (..), property, checkParallel)
import qualified Hedgehog.Gen as Gen (word8)
import qualified Hedgehog.Range as Range (constantBounded)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import qualified Trisagion.Streams.Counter as Counter (initialize)
import qualified Trisagion.Streams.Offset as Offset (initialize)

-- Testing helpers.
import Lib.Generators (genStream)
import Lib.Properties (
    prop_function_extensional_equality,
    prop_monofunctor_identity,
    prop_monofunctor_composition,
    )


-- Properties.
prop_uncons_lists
    :: (Monad m, Streamable s, Eq (ElementOf s), Show (ElementOf s), Show s)
    => Gen s
    -> PropertyT m ()
prop_uncons_lists = prop_function_extensional_equality
    toList
    (maybe [] (\ (y, ys) -> y : toList ys) . uncons)


-- Main test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Typeclasses.Streamable" [
    ("Monofunctor law identity: Counter", property $ prop_monofunctor_identity counters),
    ("Monofunctor law composition: Counter", property $ prop_monofunctor_composition word8s counters),
    ("Uncons law: Counter", property $ prop_uncons_lists counters),
    ("Monofunctor law identity: Offset", property $ prop_monofunctor_identity offsets),
    ("Monofunctor law composition: Offset", property $ prop_monofunctor_composition word8s offsets),
    ("Uncons law: Offset", property $ prop_uncons_lists offsets)
    ]
    where
        word8s = Gen.word8 Range.constantBounded
        counters = genStream Counter.initialize 10
        offsets  = genStream Offset.initialize 10
