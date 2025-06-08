module Lib.Generators (
    -- * Data types for functions.
    Shift (..),

    -- ** Functions.
    makeFunction,

    -- * Data types for parsers.
    ParserExp (..),

    -- ** Functions.
    makeParser,

    -- * Generators.
    -- ** Sizes.
    genSize,

    -- ** Bytestrings.
    genBytes,

    -- ** Functions.
    genShift,

    -- ** 'ParseError'.
    genParseError,
    genParseErrorUnit,

    -- ** Streams.
    genCounter,
    genOffset,
    genParserExp,

    -- ** Parsers.
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (absurd)
import Data.Word (Word32, Word8)

-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (word, word32, maybe, bytes, word8, choice)
import qualified Hedgehog.Range as Range (linearBounded, linear)

-- Libraries.
import Data.ByteString (ByteString)
import Optics.Core ((%), review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)
import Trisagion.Streams.Counter (Counter)
import Trisagion.Streams.Offset (Offset)
import qualified Trisagion.Streams.Counter as Counter (initialize)
import qualified Trisagion.Streams.Offset as Offset (initialize)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError)
import Trisagion.Parsers.Combinators (count)
import Trisagion.Parsers.Streamable (one)


{- | Type of the 'Num' shift functions. -}
newtype Shift a = Shift a
    deriving stock (Eq, Show, Functor)


{- | Functionalize the 'Shift' value -}
makeFunction :: Num a => Shift a -> a -> a
makeFunction (Shift x) = (x +)


{- | Simple ADT to generate parsers. -}
data ParserExp
    = Pure Word8              -- ^ Parser that always succeds and does not consume input.
    | Throw Word32            -- ^ Parser that always fails.
    | Count Word              -- ^ Parser that may or may not fail depending on input.
    deriving stock (Eq, Show)


{- | Functionalize the 'ParserExp' value -}
makeParser :: (HasOffset s, ElementOf s ~ Word8) => ParserExp -> Parser s (ParseError Word32) Word8
makeParser e = case e of
    Pure n -> pure n
    Throw n -> throwParseError n
    Count n -> foldl' (+) 0 <$> count n (first (fmap absurd) one)


-- Generators.
genSize :: Gen Word
genSize = Gen.word (Range.linear 0 12)

genBytes :: Gen ByteString
genBytes = Gen.bytes (Range.linear 0 10)

genParseError :: Gen (ParseError Word32)
genParseError = makeError <$> genSize <*> Gen.word32 Range.linearBounded
    where
        makeError offset tag = review (singleton % errorItem) (offset, tag)

genParseErrorUnit :: Gen (ParseError Word32)
genParseErrorUnit = fromMaybe mempty <$> Gen.maybe genParseError

genShift :: Gen (Shift Word32)
genShift = Shift <$> Gen.word32 Range.linearBounded

genCounter :: Gen (Counter ByteString)
genCounter = Counter.initialize <$> genBytes

genOffset :: Gen (Offset ByteString)
genOffset = Offset.initialize <$> genBytes

genParserExp :: Gen ParserExp
genParserExp = Gen.choice [genPure, genThrow, genCount]
    where
        genPure = Pure <$> Gen.word8 Range.linearBounded
        genThrow = Throw <$> Gen.word32 Range.linearBounded
        genCount = Count <$> genSize
