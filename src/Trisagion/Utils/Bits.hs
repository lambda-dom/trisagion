{- |
Module: Trisagion.Utils.Bits

Bit utilities for integral types.
-}

module Trisagion.Utils.Bits (
    -- * Bitwise functions.
    bitcount,

    -- * Bytewise functions.
    bytecount,
    pack,
    packReverse,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits (..), Bits (..))
import Data.Word (Word8)

-- Package.
import Trisagion.Utils.List (enumDown)


-- $setup
-- >>> import Data.Word


{- | Return the number of bits in the integral type.

=== __Examples:__

>>> bitcount Word8
8

>>> bitcount Int
64
-}
{-# INLINE bitcount #-}
bitcount :: forall a -> (FiniteBits a, Num a) => Int
bitcount a = finiteBitSize (0 :: a)

{- | Return the number of bytes in the integral type.

The actual argument is ignored by the function and only the type matters.

note(s):

  * It is implicitely assumed that the number of bits is a (positive) multiple of @8@.

=== __Examples:__

>>> bytecount Word8
1

>>> bytecount Int
8
-}
{-# INLINE bytecount #-}
bytecount :: forall a -> (FiniteBits a, Num a) => Int
bytecount a = bitcount a `quot` 8

{- | Shift an integral @n@ bytes left.

note(s):

  * It is implicitely assumed that @n@ is positive.
-}
{-# INLINE shiftByteL #-}
shiftByteL :: Bits a => Int -> a -> a
shiftByteL n m = shiftL m (8 * n)

{- | Pack a list of bytes into an integral value.

note(s):

  * Argument list is truncated to a list of 'bytecount' length.
-}
{-# INLINEABLE pack #-}
pack :: forall a . (FiniteBits a, Integral a) => [Word8] -> a
pack
    = foldl' (.|.) zeroBits
    . fmap (uncurry shiftByteL)
    . zip [0 .. pred $ bytecount a]
    . fmap fromIntegral

{- | Pack a list of bytes into an integral value in reverse order.

Equivalent to, but more efficient than, @'pack' . reverse@.

note(s)

  * Argument list is truncated to a list of 'bytecount' length.
-}
{-# INLINEABLE packReverse #-}
packReverse :: forall a . (FiniteBits a, Integral a) => [Word8] -> a
packReverse
        = foldl' (.|.) zeroBits
        . fmap (uncurry shiftByteL)
        . enumDown (pred $ bytecount a)
        . fmap fromIntegral
