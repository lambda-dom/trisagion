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

-- Package.
import Trisagion.HasOffset (HasOffset (..))
import Trisagion.IStream (IStream (..))
import Trisagion.Split (Split (..))
import Data.MonoFoldable (MonoFoldable (..))


{- | The 8-bit character type. -}
newtype Char8 = Char8 Word8
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum)


-- Instances.
instance Show Char8 where
    {-# INLINE show #-}
    show :: Char8 -> String
    show (Char8 n) = show . chr . review word8ToInt $ n


{- | The isomorphism @Char8 -> Word8@. -}
{-# INLINE char8 #-}
char8 :: Iso' Char8 Word8
char8 = coercedTo

{- | The prism for @Char8@. -}
{-# INLINE char #-}
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


{- | Wrapper around a binary @IStream 'Word8'@ to produce a t'Char8'-stream.

A specialization of the @fromIso@ stream combinator.
-}
newtype Chars s = Chars s
    deriving stock Eq
    deriving newtype Show


-- Instances.
instance MonoFunctor Word8 s => MonoFunctor Char8 (Chars s) where
    {-# INLINE monomap #-}
    monomap :: (Char8 -> Char8) -> Chars s -> Chars s
    monomap f (Chars xs) = Chars (monomap (view char8 . f . review char8) xs)

instance MonoPointed Word8 s => MonoPointed Char8 (Chars s) where
    {-# INLINE monopoint #-}
    monopoint :: Char8 -> Chars s
    monopoint (Char8 n) = Chars $ monopoint n

instance MonoFoldable Word8 s => MonoFoldable Char8 (Chars s) where
    {-# INLINE monotoList #-}
    monotoList :: Chars s -> [Char8]
    monotoList (Chars xs) = fmap (review char8) $ monotoList xs

    {-# INLINE monofoldMap #-}
    monofoldMap :: Monoid m => (Char8 -> m) -> Chars s -> m
    monofoldMap f (Chars xs) = monofoldMap (f . review char8) xs

    {-# INLINEABLE monofoldr #-}
    monofoldr :: (Char8 -> b -> b) -> b -> Chars s -> b
    monofoldr f x (Chars xs) = monofoldr (f . review char8) x xs

    {-# INLINEABLE monofoldl #-}
    monofoldl :: (b -> Char8 -> b) -> b -> Chars s -> b
    monofoldl f x (Chars xs) = monofoldl (\ y n -> f y (review char8 n)) x xs

    {-# INLINE mononull #-}
    mononull :: Chars s -> Bool
    mononull (Chars xs) = mononull xs

    {-# INLINE monolength #-}
    monolength :: Chars s -> Int
    monolength (Chars xs) = monolength xs

instance IStream Word8 s => IStream Char8 (Chars s) where
    {-# INLINE uncons #-}
    uncons :: Chars s -> Maybe (Char8, Chars s)
    uncons (Chars xs) =
        case uncons xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (review char8 y, Chars ys)

    {-# INLINE isNull #-}
    isNull :: Chars s -> Bool
    isNull (Chars xs) = isNull xs

    {-# INLINE toList #-}
    toList :: Chars s -> [Char8]
    toList (Chars xs) = fmap (review char8) $ toList xs

instance HasOffset s => HasOffset (Chars s) where
    {-# INLINE offset #-}
    offset :: Chars s -> Int
    offset (Chars xs) = offset xs

instance Split Word8 s => Split Char8 (Chars s) where
    {-# INLINE splitN #-}
    splitN :: Int -> Chars s -> (Chars s, Chars s)
    splitN m (Chars xs) =
        let (prefix, rest) = splitN m xs in
            (Chars prefix, Chars rest)

    {-# INLINE splitWith #-}
    splitWith :: (Char8 -> Bool) -> Chars s -> (Chars s, Chars s)
    splitWith p (Chars xs) =
        let (prefix, rest) = splitWith (p . review char8) xs in
            (Chars prefix, Chars rest)

    {-# INLINE splitExact #-}
    splitExact :: Int -> Chars s -> Maybe (Chars s, Chars s)
    splitExact m (Chars xs) =
        case splitExact m xs of
            Nothing        -> Nothing
            Just (ys, zs)  -> Just (Chars ys, Chars zs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Chars s -> Chars s -> Maybe (Chars s)
    matchPrefix (Chars xs) (Chars ys) =
        case matchPrefix xs ys of
            Nothing -> Nothing
            Just zs -> Just (Chars zs)
