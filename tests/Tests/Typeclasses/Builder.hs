module Tests.Typeclasses.Builder (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Word (Word8)

-- Libraries.
-- Testing library.
import Hedgehog (Gen, Group, checkParallel)
import qualified Hedgehog.Gen as Gen (word8)
import qualified Hedgehog.Range as Range (linearBounded)

-- Package.
-- Package testing.
import Lib.Generators (lazyByteStrings)
import Lib.Property (andM, makeGroup)
import Lib.Properties.Builder (builderLaws)
import qualified Data.ByteString.Builder as Bytes


-- Test property groups.
testBuilderLaws :: Group
testBuilderLaws = makeGroup
        "Builder laws"
        (builderLaws word8s (lazyByteStrings 10) builders)
    where
        word8s :: Gen Word8
        word8s = Gen.word8 Range.linearBounded

        -- Argument needed for inference only.
        builders :: Gen Bytes.Builder
        builders = undefined


-- Main test driver.
tests :: IO Bool
tests = andM (checkParallel <$> [testBuilderLaws])
