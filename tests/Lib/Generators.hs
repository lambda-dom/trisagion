module Lib.Generators (
    -- * Generators.
    genSize,
    genParseError,
    genStream,
) where

-- Imports.
-- Base.
import Data.Maybe (fromMaybe)

-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (word, maybe, bytes)
import qualified Hedgehog.Range as Range (linear)

-- Libraries.
import Data.ByteString (ByteString)
import Optics.Core ((%), review)

-- non-Hackage libraries.

-- Package.
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)


{- | Generate sizes. -}
genSize :: Gen Word
genSize = Gen.word (Range.linear 0 12)

{- | Generate 'ParseError' values given a generator for error tags. -}
genParseError :: Gen e -> Gen (ParseError e)
genParseError gen = fromMaybe mempty <$> Gen.maybe genError
    where
        makeError offset tag = review (singleton % errorItem) (offset, tag)
        genError = makeError <$> genSize <*> gen

{- | Generator for input streams given a stream constructor from 'ByteString'. -}
genStream :: (ByteString -> s) -> Gen s
genStream f = f <$> Gen.bytes (Range.linear 0 10)
