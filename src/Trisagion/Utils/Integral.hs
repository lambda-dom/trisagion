{- |
Module: Trisagion.Utils.Integral

Some utilities for integral types.
-}

module Trisagion.Utils.Integral (
    -- * Enumerations.
    enumDown,

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


{- | Enumerate the elements of a list downwards from @n@ to @0@.

note(s):

  * The resulting list has at most @n + 1@ elements.
-}
{-# INLINE enumDown #-}
enumDown :: Word -> [a] -> [(Word, a)]
enumDown n = zip ns
    where
        ns = if n == 0 then [0] else [n, pred n .. 0]

{- | Return the number of bits in the integral type.

The actual argument is ignored by the function and only the type matters.
-}
{-# INLINE bitCount #-}
bitCount :: FiniteBits w => w -> Word
bitCount n = fromIntegral $ finiteBitSize n

{- | Return the number of bytes in the integral type.

The actual argument is ignored by the function and only the type matters.

note(s):

  * It is implicitely assumed that the number of bits is a multiple of @8@.
-}
{-# INLINE byteCount #-}
byteCount :: FiniteBits w => w -> Word
byteCount n = bitCount n `quot` 8

{- | Shift an integral @n@ bytes left. -}
{-# INLINE shiftByteL #-}
shiftByteL :: Bits w => Word -> w -> w
shiftByteL m n = shiftL n ( 8 * fromIntegral m)

{- | Pack a list of bytes into an integral value.

note(s):

  * Argument list is truncated to a list of 'byteCount' length.
-}
{-# INLINEABLE pack #-}
pack :: forall w . (Integral w, FiniteBits w) => [Word8] -> w
pack
    = foldl' (.|.) 0
    . fmap (uncurry shiftByteL)
    . zip [0 .. pred $ byteCount @w 0]
    . fmap fromIntegral

{- | Pack a list of bytes into an integral value in reverse order.

Equivalent to, but more efficient than, @'pack' . reverse@.

note(s)

  * Argument list is truncated to a list of 'byteCount' length.
-}
{-# INLINEABLE packReverse #-}
packReverse :: forall w . (Integral w, FiniteBits w) => [Word8] -> w
packReverse
        = foldl' (.|.) 0
        . fmap (uncurry shiftByteL)
        . enumDown (pred $ byteCount @w 0)
        . fmap fromIntegral
