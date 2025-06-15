module Tests.Parser (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Word (Word32)

-- Testing library.
import Hedgehog (Gen, Group, checkParallel)
import qualified Hedgehog.Gen as Gen (ascii, string, bytes)
import qualified Hedgehog.Range as Range (linear)

-- Libraries.
import Data.ByteString (ByteString)

-- Package.
import Trisagion.Streams.Counter (Counter, initialize)
import Trisagion.Streams.Offset (Offset)

-- Package testing.
import Lib.Property (makeGroup, andM)
import Lib.Properties.Alternative (alternativeLaws, distributiveLaw)
import qualified Trisagion.Streams.Offset as Offset


-- Generators.
chars :: Gen Char
chars = Gen.ascii

streams :: Word32 -> Gen (Counter String)
streams n = initialize <$> Gen.string (Range.linear 0 (fromIntegral n)) chars

bytes :: Word32 -> Gen (Offset ByteString)
bytes n = Offset.initialize <$> Gen.bytes (Range.linear 0 (fromIntegral n))

-- Property groups.
testsParserAlternative :: Group
testsParserAlternative = makeGroup "Alternative laws for @Parser s e a@" (alternativeLaws chars (streams 10))

testsParserDistributivity :: Group
testsParserDistributivity =
    makeGroup
        "Left distributivity for @Parser s e a@"
        (distributiveLaw $ bytes 10)


-- Main test driver.
tests :: IO Bool
tests = andM (checkParallel <$> [testsParserAlternative, testsParserDistributivity])
