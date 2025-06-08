{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, left identity" #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Parser (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (empty, (<|>)))
import Data.Word (Word8, Word32)

-- Testing.
import Hedgehog (Property, Group (..), (===), checkParallel, property, forAll)
import qualified Hedgehog.Gen as Gen (word8, word32)
import qualified Hedgehog.Range as Range (linearBounded)

-- Libraries.
import Data.ByteString (ByteString)

-- Testing helpers.
import Lib.Generators (genCounter, genParserExp, makeParser, ParserExp (..))

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser, parse)
import Trisagion.Streams.Counter (Counter)


-- Properties.
prop_left_catch :: Property
prop_left_catch = property $ do
    n  <- forAll $ Gen.word8 Range.linearBounded
    ep <- forAll genParserExp
    xs <- forAll genCounter
    parse (pure n <|> makeParser ep) xs === parse (pure n) xs

prop_left_absorption :: Property
prop_left_absorption = property $ do
    -- For inference.
    let unit = empty :: Parser (Counter ByteString) (ParseError Word32) (Word8 -> Word8)

    ep <- forAll genParserExp
    xs <- forAll genCounter
    parse (unit <*> makeParser ep) xs === parse empty xs

prop_left_zero :: Property
prop_left_zero = property $ do
    ep <- forAll genParserExp
    xs <- forAll genCounter
    parse (empty >>= const (makeParser ep)) xs === parse empty xs

prop_left_identity :: Property
prop_left_identity = property $ do
    ep <- forAll $ Throw <$> Gen.word32 Range.linearBounded
    xs <- forAll genCounter
    parse (empty <|> makeParser ep) xs === parse (makeParser ep) xs


-- Test driver.
tests :: IO Bool
tests = checkParallel $ Group "Tests.Parser" [
    ("prop_left_catch", prop_left_catch),
    ("prop_left_absorption", prop_left_absorption),
    ("prop_left_zero", prop_left_zero),
    ("prop_left_identity", prop_left_identity)
    ]
