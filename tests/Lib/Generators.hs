module Lib.Generators (
    -- * Generators.
    genSize,
    genStream,
) where

-- Imports.
-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (word, bytes)
import qualified Hedgehog.Range as Range (linear)

-- Libraries.
import Data.ByteString (ByteString)


{- | Generate sizes. -}
genSize :: Gen Word
genSize = Gen.word (Range.linear 0 12)

{- | Generator for input streams given a stream constructor from 'ByteString'. -}
genStream :: (ByteString -> s) -> Gen s
genStream f = f <$> Gen.bytes (Range.linear 0 10)
