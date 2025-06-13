module Lib.Generators (
    -- * Generators.
    sizes,
    byteStrings,
    parseErrors,
) where

-- Imports.
-- Base.
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

-- Libraries.
import Data.ByteString (ByteString)
import Optics.Core ((%), review)

-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (word, bytes, maybe)
import qualified Hedgehog.Range as Range (linear)

-- Package.
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)


{- | Generate 'Word' sizes with an upper bound @n@. -}
sizes :: Word32 -> Gen Word
sizes n = Gen.word $ Range.linear 0 (fromIntegral n)

{- | Generator for 'ByteString'. -}
byteStrings
    :: Word32                 -- ^ Upper bound for the (scaling) size.
    -> Gen ByteString
byteStrings n = Gen.bytes $ Range.linear 0 (fromIntegral n)

{- | Generator for 'ParseError' values with no backtrace. -}
parseErrors :: Word32 -> Gen e -> Gen (ParseError e)
parseErrors n gen = fromMaybe mempty <$> Gen.maybe (makeError <$> sizes n <*> gen)
    where
        makeError offset tag = review (singleton % errorItem) (offset, tag)
