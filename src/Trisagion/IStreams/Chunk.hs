{- |
Module: Trisagion.IStreams.Chunk

The @Chunk@ stream.
-}

module Trisagion.IStreams.Chunk (
    -- * Streams.
    Chunk,

    -- ** Constructors.
    initialize,
) where

-- Imports.
-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))
import Trisagion.HasOffset (HasOffset (..))
import Trisagion.IStream (IStream (..))
import Trisagion.Parser (Parser, parse)


{- | Chunk a stream with a parser. -}
data Chunk s a = Chunk {-# UNPACK #-} !Int !s (Parser s () a)
    deriving stock Functor


-- Instances.
instance MonoFunctor a (Chunk s a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Chunk s a -> Chunk s a
    monomap = fmap

instance HasOffset (Chunk s a) where
    {-# INLINE offset #-}
    offset :: Chunk s a -> Int
    offset (Chunk n _ _) = n

{- | The 'IStream' instance.

note(s):

    * All 'IStream' operations must run the parser on the input, including @head@ and @isNull@.
-}
instance IStream a (Chunk s a) where
    {-# INLINE uncons #-}
    uncons :: Chunk s a -> Maybe (a, Chunk s a)
    uncons (Chunk n xs p) =
        let res = parse p xs in
            case res of
                Left  _       -> Nothing
                Right (x, ys) -> Just $ (x, Chunk (succ n) ys p)


{- | Construct a t'Chunk'-ing stream from a parser and a stream.

note(s):

    * The exact error type of the parser is irrelevant and discarded -- pass @first (const ()) p@.
-}
{-# INLINE initialize #-}
initialize :: Parser s () a -> s -> Chunk s a
initialize p xs = Chunk 0 xs p
