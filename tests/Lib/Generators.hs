module Lib.Generators (
    -- * Typeclasses.
    Function (..),

    -- * Function types.
    Shift (..),

    -- * Generators.
    genParseError,
    genParseErrorUnit,
    genShift,
    genCounter,
    genOffset,
) where

-- Imports.
-- Base.
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (word, word32, maybe, bytes)
import qualified Hedgehog.Range as Range (linearBounded, linear)

-- Libraries.
import Data.ByteString (ByteString)
import Optics.Core ((%), review)

-- Package.
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)
import Trisagion.Streams.Counter (Counter)
import Trisagion.Streams.Offset (Offset)
import qualified Trisagion.Streams.Counter as Counter (initialize)
import qualified Trisagion.Streams.Offset as Offset (initialize)


{- | Typeclass for cuntionalization. -}
class Function f a b where
    {-# MINIMAL apply #-}

    {- | Apply an @f@ to an @a@. -}
    apply :: f -> a -> b


{- | Type of the 'Num' shifgt functions. -}
newtype Shift a = Shift a
    deriving stock (Eq, Show, Functor)

-- Instances.
instance Num a => Function (Shift a) a a where
    apply (Shift x) = (x +)


-- Generators.
genParseError :: Gen (ParseError Word32)
genParseError = makeError <$> Gen.word Range.linearBounded <*> Gen.word32 Range.linearBounded
    where
        makeError offset tag = review (singleton % errorItem) (offset, tag)

genParseErrorUnit :: Gen (ParseError Word32)
genParseErrorUnit = fromMaybe mempty <$> Gen.maybe genParseError

genShift :: Gen (Shift Word32)
genShift = Shift <$> Gen.word32 Range.linearBounded

genCounter :: Gen (Counter ByteString)
genCounter = Counter.initialize <$> Gen.bytes (Range.linear 0 10)

genOffset :: Gen (Offset ByteString)
genOffset = Offset.initialize <$> Gen.bytes (Range.linear 0 10)
