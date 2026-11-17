{- |
Module: Trisagion.IStreams.Chars

Adapter for binary streams.
-}
{-# LANGUAGE UndecidableInstances #-}

module Trisagion.IStreams.Chars (
    -- * Prisms.
    char,

    -- * IStreams.
    Chars (..),
) where

-- Imports.
-- Base.
import Data.Char (chr, ord)
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', review, prism')

-- non-Hackage libraries.
import Data.Word.Optics (word8ToInt)
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoPointed (MonoPointed (..))
import Data.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.HasOffset (HasOffset (..))
import Trisagion.IStream (IStream (..))
import qualified Trisagion.IStream as IStream (head)
import Trisagion.Split (Split (..))


{- | The prism for @Char@. -}
{-# INLINE char #-}
char :: Prism' Char Word8
char = prism' construct match
    where
        construct :: Word8 -> Char
        construct = chr . review word8ToInt

        match :: Char -> Maybe Word8
        match c = let n = ord c in
            if n <= (review word8ToInt $ maxBound @Word8)
            then Just . fromIntegral @Int @Word8 $ n
            else Nothing


{- | Turn an @'IStream' 'Word8'@ into an @'IStream' 'Char'@.

note(s):

    * All 'Char' values are truncated to 8-bits. In particular, the 'MonoFunctor' and 'MonoPointed'
    instances are unlawful, with the typeclass laws failing at characters outside of the
    @[0 .. 255]@ range.
-}
type Chars :: Type -> Type
newtype Chars s = Chars s
    deriving stock Eq
    deriving newtype (Semigroup, Monoid, Show)


-- | Unsafe conversion (i. e. truncation) from 'Char' to 'Word8'. Not exported.
truncate8 :: Char -> Word8
truncate8 = fromIntegral @Int @Word8 . ord


-- Instances.
instance MonoFunctor Word8 s => MonoFunctor Char (Chars s) where
    {-# INLINE monomap #-}
    monomap :: (Char -> Char) -> Chars s -> Chars s
    monomap f (Chars xs) = Chars (monomap (truncate8 . f . review char) xs)

instance MonoPointed Word8 s => MonoPointed Char (Chars s) where
    {-# INLINE monopoint #-}
    monopoint :: Char -> Chars s
    monopoint = Chars . monopoint . truncate8

instance MonoFoldable Word8 s => MonoFoldable Char (Chars s) where
    {-# INLINE monotoList #-}
    monotoList :: Chars s -> [Char]
    monotoList (Chars xs) = fmap (review char) $ monotoList xs

    {-# INLINE monofoldMap #-}
    monofoldMap :: Monoid m => (Char -> m) -> Chars s -> m
    monofoldMap f (Chars xs) = monofoldMap (f . review char) xs

    {-# INLINEABLE monofoldr #-}
    monofoldr :: (Char -> b -> b) -> b -> Chars s -> b
    monofoldr f x (Chars xs) = monofoldr (f . review char) x xs

    {-# INLINEABLE monofoldl #-}
    monofoldl :: (b -> Char -> b) -> b -> Chars s -> b
    monofoldl f x (Chars xs) = monofoldl (\ y n -> f y (review char n)) x xs

    {-# INLINE mononull #-}
    mononull :: Chars s -> Bool
    mononull (Chars xs) = mononull xs

    {-# INLINE monolength #-}
    monolength :: Chars s -> Int
    monolength (Chars xs) = monolength xs

instance IStream Word8 s => IStream Char (Chars s) where
    {-# INLINE uncons #-}
    uncons :: Chars s -> Maybe (Char, Chars s)
    uncons (Chars xs) =
        case uncons xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (review char y, Chars ys)

    {-# INLINE head #-}
    head :: Chars s -> Maybe Char
    head (Chars xs) = fmap (review char) $ IStream.head xs

    {-# INLINE dropOne #-}
    dropOne :: Chars s -> Chars s
    dropOne (Chars xs) = Chars $ dropOne xs

    {-# INLINE isNull #-}
    isNull :: Chars s -> Bool
    isNull (Chars xs) = isNull xs

    {-# INLINE toList #-}
    toList :: Chars s -> [Char]
    toList (Chars xs) = fmap (review char) $ toList xs

instance HasOffset s => HasOffset (Chars s) where
    {-# INLINE offset #-}
    offset :: Chars s -> Int
    offset (Chars xs) = offset xs

instance Split Word8 b s => Split Char b (Chars s) where
    {-# INLINE splitN #-}
    splitN :: Int -> Chars s -> (b, Chars s)
    splitN m (Chars xs) =
        let (prefix, rest) = splitN m xs in
            (prefix, Chars rest)

    {-# INLINE splitWith #-}
    splitWith :: (Char -> Bool) -> Chars s -> (b, Chars s)
    splitWith p (Chars xs) =
        let (prefix, rest) = splitWith (p . review char) xs in
            (prefix, Chars rest)

    {-# INLINE splitExact #-}
    splitExact :: Int -> Chars s -> Maybe (b, Chars s)
    splitExact m (Chars xs) =
        case splitExact m xs of
            Nothing        -> Nothing
            Just (ys, zs)  -> Just (ys, Chars zs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: b -> Chars s -> Maybe (Chars s)
    matchPrefix xs (Chars ys) =
        case matchPrefix xs ys of
            Nothing -> Nothing
            Just zs -> Just (Chars zs)

    {-# INLINE splitRemainder #-}
    splitRemainder :: Chars s -> (b, Chars s)
    splitRemainder (Chars xs) =
        let (prefix, rest) = splitRemainder xs in
            (prefix, Chars rest)

    {-# INLINE dropN #-}
    dropN :: Int -> Chars s -> Chars s
    dropN m (Chars xs) = Chars $ dropN m xs

    {-# INLINE dropExact #-}
    dropExact :: Int -> Chars s -> Maybe (Chars s)
    dropExact m (Chars xs) = fmap Chars $ dropExact m xs

    {-# INLINE dropWith #-}
    dropWith :: (Char -> Bool) -> Chars s -> Chars s
    dropWith p (Chars xs) = Chars $ dropWith (p . review char) xs
