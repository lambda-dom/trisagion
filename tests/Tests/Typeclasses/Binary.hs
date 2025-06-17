module Tests.Typeclasses.Binary (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Int (Int8)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Data.ByteString (fromStrict)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

-- Testing library.
import Hedgehog (Group, Gen, checkParallel)
import qualified Hedgehog.Gen as Gen (int8, word8, word16, word32, word64, bytes)
import qualified Hedgehog.Range as Range (constantBounded, linear)

-- Package.
import Trisagion.Typeclasses.Binary as Serializers (Binary (..))
import Trisagion.Parsers.Word8 as Parsers (word16Le, word32Le, word64Le, word16Be, word32Be, word64Be, int8)
import qualified Trisagion.Parsers.Streamable as Parsers (one)
import Trisagion.Serializer (Serializer, embed)

-- Package testing.
import Lib.Property (andM, makeGroup)
import Lib.Properties.Adjoints (adjointParserLaws)
import Trisagion.Typeclasses.Builder (one)


-- Ensure stream has length >= to be able to satisfy the request.
lazyByteStrings :: Gen Lazy.ByteString
lazyByteStrings = fromStrict <$> Gen.bytes (Range.linear 8 16)


-- Testing groups.
testWord8Adjoint :: Group
testWord8Adjoint =
        makeGroup
            "word8 adjointness"
            (adjointParserLaws
                Parsers.one
                (embed one :: Serializer Builder Word8)
                lazyByteStrings
                (Gen.word8 Range.constantBounded))

testWord16LeAdjoint :: Group
testWord16LeAdjoint =
        makeGroup
            "word16Le adjointness"
            (adjointParserLaws
                Parsers.word16Le
                (embed Serializers.word16Le :: Serializer Builder Word16)
                lazyByteStrings
                (Gen.word16 Range.constantBounded))

testWord32LeAdjoint :: Group
testWord32LeAdjoint =
        makeGroup
            "word32Le adjointness"
            (adjointParserLaws
                Parsers.word32Le
                (embed Serializers.word32Le :: Serializer Builder Word32)
                lazyByteStrings
                (Gen.word32 Range.constantBounded))

testWord64LeAdjoint :: Group
testWord64LeAdjoint =
        makeGroup
            "word64Le adjointness"
            (adjointParserLaws
                Parsers.word64Le
                (embed Serializers.word64Le :: Serializer Builder Word64)
                lazyByteStrings
                (Gen.word64 Range.constantBounded))

testWord16BeAdjoint :: Group
testWord16BeAdjoint =
        makeGroup
            "word16Be adjointness"
            (adjointParserLaws
                Parsers.word16Be
                (embed Serializers.word16Be :: Serializer Builder Word16)
                lazyByteStrings
                (Gen.word16 Range.constantBounded))

testWord32BeAdjoint :: Group
testWord32BeAdjoint =
        makeGroup
            "word32Be adjointness"
            (adjointParserLaws
                Parsers.word32Be
                (embed Serializers.word32Be :: Serializer Builder Word32)
                lazyByteStrings
                (Gen.word32 Range.constantBounded))

testWord64BeAdjoint :: Group
testWord64BeAdjoint =
        makeGroup
            "word64Be adjointness"
            (adjointParserLaws
                Parsers.word64Be
                (embed Serializers.word64Be :: Serializer Builder Word64)
                lazyByteStrings
                (Gen.word64 Range.constantBounded))

testInt8Adjoint :: Group
testInt8Adjoint =
        makeGroup
            "int8 adjointness"
            (adjointParserLaws
                Parsers.int8
                (embed Serializers.int8 :: Serializer Builder Int8)
                lazyByteStrings
                (Gen.int8 Range.constantBounded))


-- Main test driver.
tests :: IO Bool
tests = andM (checkParallel <$> groups)
    where
        groups = [
            testWord8Adjoint,
            testWord16LeAdjoint,
            testWord32LeAdjoint,
            testWord64LeAdjoint,
            testWord16BeAdjoint,
            testWord32BeAdjoint,
            testWord64BeAdjoint,
            testInt8Adjoint
            ]
