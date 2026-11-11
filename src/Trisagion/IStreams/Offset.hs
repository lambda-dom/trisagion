{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Trisagion.IStreams.Offset

The @Offset@ type wrapping an 'IStream' with an offset tracking current position.
-}

module Trisagion.IStreams.Offset (
    -- * IStreams.
    Offset,

    -- ** Constructors.
    initialize,
) where

-- Imports.
-- Base.
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Except (MonadError (..))

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoPointed (MonoPointed (..))
import Data.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.IStream (IStream (..))
import Trisagion.HasOffset (HasOffset (..))
import Trisagion.Split (Split (..))
import Trisagion.Parsers.Binary (Binary (..))
import Trisagion.Parser (Parser, parse)
import Trisagion.Parsers.IStream (InputError)


{- | Wrapper around an 'IStream' adding an offset to track current position.

The implementation initializes the offset to the length of the stream and on each call to
'offset' takes the difference. This not only requires efficient implementation of 'monolength',
but forces the entire input stream into memory.
-}
type Offset :: Type -> Type
data Offset s = Offset {-# UNPACK #-} !Int !s
    deriving stock (Eq, Show)


-- Instances.
instance MonoFunctor a s => MonoFunctor a (Offset s) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Offset s -> Offset s
    monomap f (Offset n xs) = Offset n (monomap f xs)

instance MonoPointed a s => MonoPointed a (Offset s) where
    {-# INLINE monopoint #-}
    monopoint :: a -> Offset s
    monopoint x = Offset 1 (monopoint x)

instance IStream a s => IStream a (Offset s) where
    {-# INLINE uncons #-}
    uncons :: Offset s -> Maybe (a, Offset s)
    uncons (Offset n xs) =
        case uncons xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (y, Offset n ys)

    {-# INLINE isNull #-}
    isNull :: Offset s -> Bool
    isNull (Offset _ xs) = isNull xs

    {-# INLINE toList #-}
    toList :: Offset s -> [a]
    toList (Offset _ xs) = toList xs

instance (IStream a s, MonoFoldable a s) => HasOffset (Offset s) where
    {-# INLINE offset #-}
    offset :: Offset s -> Int
    offset (Offset n xs) = n - monolength xs

instance (Split a b s, MonoFoldable a b) => Split a b (Offset s) where
    {-# INLINE splitN #-}
    splitN :: Int -> Offset s -> (b, Offset s)
    splitN m (Offset n xs) =
        let (prefix, rest) = splitN m xs in
            (prefix, Offset n rest)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Offset s -> (b, Offset s)
    splitWith p (Offset n xs) =
        let (prefix, rest) = splitWith p xs in
            (prefix, Offset n rest)

    {-# INLINE splitExact #-}
    splitExact :: Int -> Offset s -> Maybe (b, Offset s)
    splitExact m (Offset n xs) =
        case splitExact m xs of
            Nothing        -> Nothing
            Just (ys, zs)  -> Just (ys, Offset n zs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: b -> Offset s -> Maybe (Offset s)
    matchPrefix xs (Offset n ys) =
        case matchPrefix xs ys of
            Nothing -> Nothing
            Just zs -> Just (Offset n zs)

    {-# INLINE splitRemainder #-}
    splitRemainder :: Offset s -> (b, Offset s)
    splitRemainder (Offset n xs) =
        let (prefix, rest) = splitRemainder xs in
            (prefix, Offset n rest)

    {-# INLINE dropExact #-}
    dropExact :: Int -> Offset s -> Maybe (Offset s)
    dropExact m (Offset n xs) = fmap (Offset n) $ dropExact m xs

instance (Binary b s, MonoFoldable Word8 b) => Binary b (Offset s) where
    {-# INLINE word16Le #-}
    word16Le :: Parser (Offset s) InputError Word16
    word16Le = do
        (Offset n xs) <- get
        case parse word16Le xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Offset n ys) $> y

    {-# INLINE word32Le #-}
    word32Le :: Parser (Offset s) InputError Word32
    word32Le = do
        (Offset n xs) <- get
        case parse word32Le xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Offset n ys) $> y

    {-# INLINE word64Le #-}
    word64Le :: Parser (Offset s) InputError Word64
    word64Le = do
        (Offset n xs) <- get
        case parse word64Le xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Offset n ys) $> y

    {-# INLINE word16Be #-}
    word16Be :: Parser (Offset s) InputError Word16
    word16Be = do
        (Offset n xs) <- get
        case parse word16Be xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Offset n ys) $> y

    {-# INLINE word32Be #-}
    word32Be :: Parser (Offset s) InputError Word32
    word32Be = do
        (Offset n xs) <- get
        case parse word32Be xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Offset n ys) $> y

    {-# INLINE word64Be #-}
    word64Be :: Parser (Offset s) InputError Word64
    word64Be = do
        (Offset off xs) <- get
        case parse word64Be xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Offset off ys) $> y


{- | Construct an t'Offset' from an 'IStream'. -}
{-# INLINE initialize #-}
initialize :: MonoFoldable b s => s -> Offset s
initialize xs = Offset (monolength xs) xs
