{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, left identity" #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Parser (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative ((<|>)), empty)
import Data.Word (Word8)

-- Testing.
import Hedgehog (Property, Gen, Group (..), (===), checkParallel, property, forAll)
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
    parse (pure x <|> makeParser f h p) xs === parse (pure x) xs

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
    parse (unit <*> makeParser f h p) xs === parse empty xs

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
    parse (empty >>= const (makeParser f h p)) xs === parse empty xs

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
    parse (empty <|> makeParser f h p) xs === parse (makeParser f h p) xs


-- Test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Parser" [
        ("prop_left_catch", prop_left_catch id max gen gen (genStream initialize)),
        ("prop_left_absorption", prop_left_absorption id max gen gen (genStream initialize)),
        ("prop_left_zero", prop_left_zero id max gen gen (genStream initialize)),
        ("prop_left_identity", prop_left_identity id max gen gen (genStream initialize))
        ]
    where
        gen = Gen.word8 Range.linearBounded
