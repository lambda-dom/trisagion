{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Trisagion.IStreams.Counter

The @Counter@ type wrapping an 'IStream' with an offset tracking the current position.
-}

module Trisagion.IStreams.Counter (
    -- * IStreams.
    Counter,

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

The implementation initializes the counter to @0@ and then updates it on every operation of the
input stream by computing the length of the prefix.
-}
type Counter :: Type -> Type
data Counter s = Counter {-# UNPACK #-} !Int !s
    deriving stock (Eq, Show)


-- Instances.
instance MonoFunctor a s => MonoFunctor a (Counter s) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Counter s -> Counter s
    monomap f (Counter n xs) = Counter n (monomap f xs)

instance MonoPointed a s => MonoPointed a (Counter s) where
    {-# INLINE monopoint #-}
    monopoint :: a -> Counter s
    monopoint x = Counter 0 (monopoint x)

instance IStream a s => IStream a (Counter s) where
    {-# INLINE uncons #-}
    uncons :: Counter s -> Maybe (a, Counter s)
    uncons (Counter n xs) =
        case uncons xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (y, Counter (succ n) ys)

    {-# INLINE isNull #-}
    isNull :: Counter s -> Bool
    isNull (Counter _ xs) = isNull xs

    {-# INLINE toList #-}
    toList :: Counter s -> [a]
    toList (Counter _ xs) = toList xs

instance IStream a s => HasOffset (Counter s) where
    {-# INLINE offset #-}
    offset :: Counter s -> Int
    offset (Counter n _) = n

{- | 'Split' instance.

The instance requires computing the length of the prefix, which is @O(n)@ for some types like
@Text@. This in its turn, requires a @'MonoFoldable' a b@ constraint and the
@UndecidableInstances@ extension to shut up GHC.
-}
instance (Split a b s, MonoFoldable a b) => Split a b (Counter s) where
    {-# INLINE splitN #-}
    splitN :: Int -> Counter s -> (b, Counter s)
    splitN m (Counter n xs) =
        let (prefix, rest) = splitN m xs in
            (prefix, Counter (n + monolength prefix) rest)

    {-# INLINE splitWith #-}
    splitWith :: (a -> Bool) -> Counter s -> (b, Counter s)
    splitWith p (Counter n xs) =
        let (prefix, rest) = splitWith p xs in
            (prefix, Counter (n + monolength prefix) rest)

    {-# INLINE splitExact #-}
    splitExact :: Int -> Counter s -> Maybe (b, Counter s)
    splitExact m (Counter n xs) =
        case splitExact m xs of
            Nothing        -> Nothing
            Just (ys, zs)  -> Just (ys, Counter (n + monolength ys) zs)

    {-# INLINE matchPrefix #-}
    matchPrefix :: b -> Counter s -> Maybe (Counter s)
    matchPrefix xs (Counter n ys) =
        case matchPrefix xs ys of
            Nothing -> Nothing
            Just zs -> Just (Counter (n + monolength xs) zs)

    {-# INLINE splitRemainder #-}
    splitRemainder :: Counter s -> (b, Counter s)
    splitRemainder (Counter n xs) =
        let (prefix, rest) = splitRemainder xs in
            (prefix, Counter (n + monolength prefix) rest)

    {-# INLINE dropExact #-}
    dropExact :: Int -> Counter s -> Maybe (Counter s)
    dropExact m (Counter off xs) = fmap (Counter (off + m)) $ dropExact m xs

instance (Binary b s, MonoFoldable Word8 b) => Binary b (Counter s) where
    {-# INLINE word16Le #-}
    word16Le :: Parser (Counter s) InputError Word16
    word16Le = do
        (Counter off xs) <- get
        case parse word16Le xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Counter (off + 2) ys) $> y

    {-# INLINE word32Le #-}
    word32Le :: Parser (Counter s) InputError Word32
    word32Le = do
        (Counter off xs) <- get
        case parse word32Le xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Counter (off + 4) ys) $> y

    {-# INLINE word64Le #-}
    word64Le :: Parser (Counter s) InputError Word64
    word64Le = do
        (Counter off xs) <- get
        case parse word64Le xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Counter (off + 8) ys) $> y

    {-# INLINE word16Be #-}
    word16Be :: Parser (Counter s) InputError Word16
    word16Be = do
        (Counter off xs) <- get
        case parse word16Be xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Counter (off + 2) ys) $> y

    {-# INLINE word32Be #-}
    word32Be :: Parser (Counter s) InputError Word32
    word32Be = do
        (Counter off xs) <- get
        case parse word32Be xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Counter (off + 4) ys) $> y

    {-# INLINE word64Be #-}
    word64Be :: Parser (Counter s) InputError Word64
    word64Be = do
        (Counter off xs) <- get
        case parse word64Be xs of
            Left e        -> throwError e
            Right (y, ys) -> put (Counter (off + 8) ys) $> y


{- | Construct a t'Counter' from a 'IStream'. -}
{-# INLINE initialize #-}
initialize :: s -> Counter s
initialize = Counter 0
