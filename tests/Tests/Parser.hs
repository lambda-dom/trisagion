{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, left identity" #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Parser (
    -- Parser equality helper.
    testEqual,

    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative ((<|>)), empty)
import Data.Word (Word8)

-- Testing.
import Hedgehog (Property, Gen, Group (..), (===), checkParallel, property, forAll, MonadTest)
import qualified Hedgehog.Gen as Gen (word8)
import qualified Hedgehog.Range as Range (linearBounded)

-- non-Hackage Libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Testing helpers.
import Lib.Generators (genStream)
import Lib.ParserExp (genParserExp, makeParser)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Streams.Counter (initialize)
import Trisagion.Parser (parse, Parser)
import Lib.FunctionExp (makeParserFunctionExp, makeFunction)
import Trisagion.Parsers.Streamable (one)
import Data.Void (absurd)
import Data.Bifunctor (Bifunctor(..))


-- Property function helper.
testEqual
    :: (MonadTest m, Eq s, Show s, Eq e, Show e, Eq a, Show a)
    => Parser s e a
    -> Parser s e a
    -> s
    -> m ()
testEqual p q xs = parse p xs === parse q xs


-- Properties.
prop_left_catch
    :: (HasOffset s, Show s, Eq s, ElementOf s ~ Word8, Eq a, Show a, Eq e, Show e)
    => (Word8 -> a)
    -> (a -> Word8 -> a)
    -> Gen e
    -> Gen a
    -> Gen s
    -> Property
prop_left_catch f h genE genA genS = property $ do
    x <- forAll genA
    p <- forAll (genParserExp genE genA)
    xs <- forAll genS
    testEqual
        (pure x <|> makeParser f h p)
        (pure x)
        xs

prop_left_absorption
    :: forall s e a . (HasOffset s, Show s, Eq s, ElementOf s ~ Word8, Eq a, Show a, Eq e, Show e)
    => (Word8 -> a)
    -> (a -> Word8 -> a)
    -> Gen e
    -> Gen a
    -> Gen s
    -> Property
prop_left_absorption f h genE genA genS = property $ do
    -- Needed for inference.
    let unit = empty :: Parser s (ParseError e) (a -> a)

    p <- forAll (genParserExp genE genA)
    xs <- forAll genS
    testEqual
        (unit <*> makeParser f h p)
        empty
        xs

prop_left_zero
    :: (HasOffset s, Show s, Eq s, ElementOf s ~ Word8, Eq a, Show a, Eq e, Show e)
    => (Word8 -> a)
    -> (a -> Word8 -> a)
    -> Gen e
    -> Gen a
    -> Gen s
    -> Property
prop_left_zero f h genE genA genS = property $ do
    p <- forAll (genParserExp genE genA)
    xs <- forAll genS
    testEqual
        (empty >>= const (makeParser f h p))
        empty
        xs

prop_left_identity
    :: (HasOffset s, Show s, Eq s, ElementOf s ~ Word8, Eq a, Show a, Eq e, Show e)
    => (Word8 -> a)
    -> (a -> Word8 -> a)
    -> Gen e
    -> Gen a
    -> Gen s
    -> Property
prop_left_identity f h genE genA genS = property $ do
    p <- forAll (genParserExp genE genA)
    xs <- forAll genS
    testEqual
        (empty <|> makeParser f h p)
        (makeParser f h p)
        xs

prop_left_distributivity
    :: forall s e a . (HasOffset s, Show s, Eq s, ElementOf s ~ Word8, Eq a, Show a, Eq e, Show e)
    => Parser s (ParseError e) a
    -> (a -> a -> a)
    -> (Word8 -> a)
    -> (a -> Word8 -> a)
    -> Gen e
    -> Gen a
    -> Gen s
    -> Property
prop_left_distributivity p j m h genE genA genS = property $ do
    -- Parser that parses out function expressions.
    let f = makeFunction j <$> makeParserFunctionExp p

    x <- forAll (genParserExp genE genA)
    y <- forAll (genParserExp genE genA)
    xs <- forAll genS
    testEqual
        (f <*> (makeParser m h x <|> makeParser m h y))
        ((f <*> makeParser m h x) <|> (f <*> makeParser m h y))
        xs

-- Test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Parser" [
        ("prop_left_catch", prop_left_catch id max genA genA genS),
        ("prop_left_absorption", prop_left_absorption id max genA genA genS),
        ("prop_left_zero", prop_left_zero id max genA genA genS),
        ("prop_left_identity", prop_left_identity id max genA genA genS),
        ("prop_left_distributivity", prop_left_distributivity p (+) id max genA genA genS)
        ]
    where
        genA = Gen.word8 Range.linearBounded
        genS = genStream initialize
        p = first (fmap absurd) one
