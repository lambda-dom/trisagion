{- | Some generators used in testing. -}

module Lib.Gen (
    -- * Generators for integral, primitive types.
    integral,
    word8,

    -- * Generators.
    parseError,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Word (Word8)

-- Libraries.
-- Testing.
import Test.Falsify (Gen)
import Test.Falsify.Generator (inRange, shrinkToNothing, int)
import Test.Falsify.Range (uniform)

-- Package.
import Trisagion.Types.ParseError (ParseError (..))


{- | Generator for bounded integral types with a finite number of bits. Biased towards zero. -}
integral :: forall a . (Integral a, Bounded a, FiniteBits a) => Gen a
integral = inRange $ uniform @a

{- | Specialization of 'integral' to 'Word8'. -}
word8 :: Gen Word8
word8 = integral


{- | Generator for 'ParseError'. -}
parseError :: Gen m -> Gen (ParseError m)
parseError g = do
    x <- shrinkToNothing g
    case x of
        Nothing -> pure Failure
        Just y  -> do
            n <- int $ uniform
            pure $ ParseError n y
