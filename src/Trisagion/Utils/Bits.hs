{- |
Module: Trisagion.Utils.Bits

Bit utilities for integral types.
-}

module Trisagion.Utils.Bits (
    -- * Bitwise functions.
    bitCount,

    -- * Bytewise functions.
    byteCount,
    pack,
    packReverse,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits (..), Bits (..))
import Data.Word (Word8)

-- Package.
import Trisagion.Utils.List (enumDown)


{- | Return the number of bits in the integral type.

The actual argument is ignored by the function and only the type matters.
-}
{-# INLINE bitCount #-}
bitCount :: FiniteBits w => w -> Int
bitCount = finiteBitSize

{- | Return the number of bytes in the integral type.

The actual argument is ignored by the function and only the type matters.

note(s):

  * It is implicitely assumed that the number of bits is a (positive) multiple of @8@.
-}
{-# INLINE byteCount #-}
byteCount :: FiniteBits w => w -> Int
byteCount n = bitCount n `quot` 8

{- | Shift an integral @n@ bytes left.

note(s):

  * It is implicitely assumed that @n@ is positive.
-}
{-# INLINE shiftByteL #-}
shiftByteL :: Bits w => Int -> w -> w
shiftByteL n m = shiftL m (8 * n)

{- | Pack a list of bytes into an integral value.

note(s):

  * Argument list is truncated to a list of 'byteCount' length.
-}
{-# INLINEABLE pack #-}
pack :: forall w . (FiniteBits w, Integral w) => [Word8] -> w
pack
    = foldl' (.|.) zeroBits
    . fmap (uncurry shiftByteL)
    . zip [0 .. pred $ byteCount @w zeroBits]
    . fmap fromIntegral

{- | Pack a list of bytes into an integral value in reverse order.

Equivalent to, but more efficient than, @'pack' . reverse@.

note(s)

  * Argument list is truncated to a list of 'byteCount' length.
-}
{-# INLINEABLE packReverse #-}
packReverse :: forall w . (FiniteBits w, Integral w) => [Word8] -> w
packReverse
        = foldl' (.|.) zeroBits
        . fmap (uncurry shiftByteL)
        . enumDown (pred $ byteCount @w zeroBits)
        . fmap fromIntegral
