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
genSize :: Word -> Gen Word
genSize n = Gen.word (Range.linear 0 n)

{- | Generator for binary input streams given a stream constructor for 'ByteString'. -}
genStream
    :: (ByteString -> s)      -- ^ Stream constructor.
    -> Word                   -- ^ Upper bound for the (scaling) size.
    -> Gen s
genStream f n = f <$> Gen.bytes (Range.linear 0 (fromIntegral n))
