module Tests.Types.Counter (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Word (Word8)

-- Libraries.
import Data.ByteString (ByteString)

-- Testing library.
import Hedgehog (Group (..), Gen, checkParallel)
import qualified Hedgehog.Gen as Gen (word8, bytes)
import qualified Hedgehog.Range as Range (linearBounded, linear)

-- Package testing.
import Lib.Property (makeGroup, andM)
import Lib.Properties.Streamable (streamableLaws)
import Lib.Properties.Splittable (splittableLaws)

-- Package.
import Trisagion.Streams.Counter (Counter)
import qualified Trisagion.Streams.Counter as Counter (initialize)


-- Generators.
word8s :: Gen Word8
word8s = Gen.word8 Range.linearBounded

streams :: Gen (Counter ByteString)
streams = Counter.initialize <$> Gen.bytes (Range.linear 0 10)

-- Property groups.
testStreamable :: Group
testStreamable = makeGroup $ streamableLaws word8s streams

testSplittable :: Group
testSplittable = makeGroup $ splittableLaws 10 word8s streams


-- Main test driver.
tests :: IO Bool
tests = andM (checkParallel <$> [testStreamable, testSplittable])
