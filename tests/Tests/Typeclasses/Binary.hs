module Tests.Typeclasses.Binary (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Int (Int8)
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Builder (Builder)

-- Testing library.
import Hedgehog (Group, checkParallel)
import qualified Hedgehog.Gen as Gen (int8, word8)
import qualified Hedgehog.Range as Range (constantBounded)

-- Package.
import Trisagion.Typeclasses.Binary as Serializers (Binary (..))
import Trisagion.Parsers.Word8 as Parsers (int8)
import qualified Trisagion.Parsers.Streamable as Parsers (one)
import Trisagion.Serializer (Serializer, embed)

-- Package testing.
import Lib.Generators (lazyByteStrings)
import Lib.Property (andM, makeGroup)
import Lib.Properties.Adjoints (adjointParserLaws)
import Trisagion.Typeclasses.Builder (one)


-- Testing groups.
testWord8Adjoint :: Group
testWord8Adjoint =
        makeGroup
            "word8 adjointness"
            (adjointParserLaws
                Parsers.one
                (embed one :: Serializer Builder Word8)
                (lazyByteStrings 10)
                (Gen.word8 Range.constantBounded))

testInt8Adjoint :: Group
testInt8Adjoint =
        makeGroup
            "int8 adjointness"
            (adjointParserLaws
                Parsers.int8
                (embed Serializers.int8 :: Serializer Builder Int8)
                (lazyByteStrings 10)
                (Gen.int8 Range.constantBounded))


-- Main test driver.
tests :: IO Bool
tests = andM (checkParallel <$> groups)
    where
        groups = [
            testWord8Adjoint,
            testInt8Adjoint
            ]
