module Lib.ParserExp (
    -- * Types.
    ParserExp (..),

    -- ** Functions.
    makeParser,

    -- ** Generators.
    genParserExp,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List (foldl')
import Data.Void (absurd)
import Data.Word (Word8)

-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (constant, choice)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError)
import Trisagion.Parsers.Combinators (count)
import Trisagion.Parsers.Streamable (one)

-- Testing helpers.
import Lib.Generators (genSize)


{- | Simple ADT to generate parsers. -}
data ParserExp e a
    = One                               -- ^ Parser that almost always succeeds and consuming input.
    | Pure a                            -- ^ Parser that always succeds and does not consume input.
    | Throw e                           -- ^ Parser that always fails.
    | Count a Word                      -- ^ Parser that may or may not fail depending on input.
    deriving stock (Eq, Show)


{- | Functionalize the 'ParserExp' value -}
makeParser
    :: (HasOffset s, ElementOf s ~ Word8)
    => (Word8 -> a)                     -- ^ Mapper to use with one.
    -> (a -> Word8 -> a)                -- ^ Folder to use with count.
    -> ParserExp e a
    -> Parser s (ParseError e) a
makeParser f _ One         = bimap (fmap absurd) f one
makeParser _ _ (Pure x)    = pure x
makeParser _ _ (Throw e)   = throwParseError e
makeParser _ h (Count x n) = foldl' h x <$> count n (first (fmap absurd) one)

-- Generators.
genOne :: Gen (ParserExp e a)
genOne = Gen.constant One

genPure :: Gen a -> Gen (ParserExp e a)
genPure = fmap Pure

genThrow :: Gen e -> Gen (ParserExp e a)
genThrow = fmap Throw

genCount :: Gen a -> Gen (ParserExp e a)
genCount gen = Count <$> gen <*> genSize

{- | Generator for 'ParserExp'. -}
genParserExp :: Gen e -> Gen a -> Gen (ParserExp e a)
genParserExp genE genA = Gen.choice [genOne, genPure genA, genThrow genE, genCount genA]
