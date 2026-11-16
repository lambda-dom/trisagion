{- |
Module: Trisagion.IStreams.Char8

Adapter for binary streams.
-}

module Trisagion.IStreams.Char8 (
    -- * The @Char8@ character type.
    Char8,

    -- ** Isomorphisms and prisms.
    char8,
    char,

    -- * IStreams.
    Char8s,
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
newtype Char8s s = Char8s s
    deriving stock Eq
    deriving newtype Show


-- Instances.
instance MonoFunctor Word8 s => MonoFunctor Char8 (Char8s s) where
    {-# INLINE monomap #-}
    monomap :: (Char8 -> Char8) -> Char8s s -> Char8s s
    monomap f (Char8s xs) = Char8s (monomap (view char8 . f . review char8) xs)

instance MonoPointed Word8 s => MonoPointed Char8 (Char8s s) where
    {-# INLINE monopoint #-}
    monopoint :: Char8 -> Char8s s
    monopoint (Char8 n) = Char8s $ monopoint n

instance MonoFoldable Word8 s => MonoFoldable Char8 (Char8s s) where
    {-# INLINE monotoList #-}
    monotoList :: Char8s s -> [Char8]
    monotoList (Char8s xs) = fmap (review char8) $ monotoList xs

    {-# INLINE monofoldMap #-}
    monofoldMap :: Monoid m => (Char8 -> m) -> Char8s s -> m
    monofoldMap f (Char8s xs) = monofoldMap (f . review char8) xs

    {-# INLINEABLE monofoldr #-}
    monofoldr :: (Char8 -> b -> b) -> b -> Char8s s -> b
    monofoldr f x (Char8s xs) = monofoldr (f . review char8) x xs

    {-# INLINEABLE monofoldl #-}
    monofoldl :: (b -> Char8 -> b) -> b -> Char8s s -> b
    monofoldl f x (Char8s xs) = monofoldl (\ y n -> f y (review char8 n)) x xs

    {-# INLINE mononull #-}
    mononull :: Char8s s -> Bool
    mononull (Char8s xs) = mononull xs

    {-# INLINE monolength #-}
    monolength :: Char8s s -> Int
    monolength (Char8s xs) = monolength xs

instance IStream Word8 s => IStream Char8 (Char8s s) where
    {-# INLINE uncons #-}
    uncons :: Char8s s -> Maybe (Char8, Char8s s)
    uncons (Char8s xs) =
        case uncons xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (review char8 y, Char8s ys)

    {-# INLINE isNull #-}
    isNull :: Char8s s -> Bool
    isNull (Char8s xs) = isNull xs

    {-# INLINE toList #-}
    toList :: Char8s s -> [Char8]
    toList (Char8s xs) = fmap (review char8) $ toList xs

instance HasOffset s => HasOffset (Char8s s) where
    {-# INLINE offset #-}
    offset :: Char8s s -> Int
    offset (Char8s xs) = offset xs

instance Split Word8 s => Split Char8 (Char8s s) where
    {-# INLINE splitN #-}
    splitN :: Int -> Char8s s -> (Char8s s, Char8s s)
    splitN m (Char8s xs) =
        let (prefix, rest) = splitN m xs in
            (Char8s prefix, Char8s rest)

    {-# INLINE splitWith #-}
    splitWith :: (Char8 -> Bool) -> Char8s s -> (Char8s s, Char8s s)
    splitWith p (Char8s xs) =
        let (prefix, rest) = splitWith (p . review char8) xs in
            (Char8s prefix, Char8s rest)

    {-# INLINE splitExact #-}
    splitExact :: Int -> Char8s s -> Maybe (Char8s s, Char8s s)
    splitExact m (Char8s xs) =
        case splitExact m xs of
            Nothing        -> Nothing
            Just (ys, zs)  -> Just (Char8s ys, Char8s zs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: Char8s s -> Char8s s -> Maybe (Char8s s)
    matchPrefix (Char8s xs) (Char8s ys) =
        case matchPrefix xs ys of
            Nothing -> Nothing
            Just zs -> Just (Char8s zs)
