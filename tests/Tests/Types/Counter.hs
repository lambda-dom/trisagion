module Tests.Types.Counter (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing library.
import Hedgehog (Group (..), Gen, checkParallel)
import qualified Hedgehog.Gen as Gen (ascii, string)
import qualified Hedgehog.Range as Range (linear)

-- Package testing.
import Lib.Property (makeGroup, andM)
import Lib.Properties.Streamable (streamableLaws)
import Lib.Properties.Splittable (splittableLaws)

-- Package.
import Trisagion.Streams.Counter (Counter)
import qualified Trisagion.Streams.Counter as Counter (initialize)


-- Generators.
streams :: Gen (Counter String)
streams = Counter.initialize <$> Gen.string (Range.linear 0 10) Gen.ascii

-- Property groups.
testStreamable :: Group
testStreamable = makeGroup $ streamableLaws Gen.ascii streams

testSplittable :: Group
testSplittable = makeGroup $ splittableLaws 10 Gen.ascii streams


-- Main test driver.
tests :: IO Bool
tests = andM (checkParallel <$> [testStreamable, testSplittable])
