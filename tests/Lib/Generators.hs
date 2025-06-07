module Lib.Generators (
    -- * Typeclasses.
    Function (..),

    -- * Function types.
    Shift (..),

    -- * Generators.
    genParseError,
    genParseErrorUnit,
    genShift,
) where

-- Imports.
-- Base.
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (word, word32, maybe)
import qualified Hedgehog.Range as Range (linearBounded)

-- Libraries.
import Optics.Core ((%), review)

-- Package.
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)


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
