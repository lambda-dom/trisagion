{- |
Module: Trisagion.IStreams.Char8

Adapter for binary streams.
-}

module Trisagion.IStreams.Char8 (
    -- * The @Char8@ character type.
    Char8,

    -- ** Isomorphisms and prisms.
    char8,
    char

    -- * IStreams.
) where

-- Imports.
-- Base.
import Data.Char (chr, ord)
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Iso', Prism', coercedTo, review, prism', view)

-- non-Hackage libraries.
import Data.Word.Optics (word8ToInt)
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoPointed (MonoPointed (..))


{- | The 8-bit character type. -}
newtype Char8 = Char8 Word8
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum)


-- Instances.
instance Show Char8 where
    show :: Char8 -> String
    show (Char8 n) = show . chr . review word8ToInt $ n


{- | The isomorphism @Char8 -> Word8@. -}
char8 :: Iso' Char8 Word8
char8 = coercedTo

{- | The prism for @Char8@. -}
char :: Prism' Char Char8
char = prism' construct match
    where
        construct :: Char8 -> Char
        construct (Char8 n) = chr . review word8ToInt $ n

        match :: Char -> Maybe Char8
        match c = let n = ord c in
            if n <= (review word8ToInt $ maxBound @Word8)
            then Just . Char8 . fromIntegral @Int @Word8 $ n
            else Nothing


{- | Wrapper around a binary stream @IStream 'Word8'@ to produce a t'Char8'-stream. -}
newtype Chars s = Chars s
    deriving stock Eq
    deriving newtype Show

-- Instances.
instance MonoFunctor Word8 s => MonoFunctor Char8 (Chars s) where
    {-# INLINE monomap #-}
    monomap :: (Char8 -> Char8) -> Chars s -> Chars s
    monomap f (Chars xs) = Chars (monomap (view char8 . f . review char8) xs)

instance MonoPointed Word8 s => MonoPointed Char8 (Chars s) where
    monopoint :: Char8 -> Chars s
    monopoint (Char8 n) = Chars $ monopoint n
