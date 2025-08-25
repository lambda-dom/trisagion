{- |
Module: Trisagion.Streams.Chunk

The @Chunk@ stream.
-}

module Trisagion.Streams.Chunk (
    -- * Streams.
    Chunk,

    -- ** Constructors.
    initialize,
) where

-- Imports.
-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Parser (Parser, parse)


{- | Chunk a stream with a parser. -}
data Chunk s e a = Chunk {-# UNPACK #-} !Word !s (Parser s e a)
    deriving stock Functor

-- Instances.
instance MonoFunctor (Chunk s e a) where
    {- | The type of the elements of a chunked stream. -}
    type ElementOf (Chunk s e a) = a

    {- | Monomorphic map over a chunked stream. -}
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Chunk s e a -> Chunk s e a
    monomap = fmap

instance Streamable (Chunk s e a) where
    {-# INLINE uncons #-}
    uncons :: Chunk s e a -> Maybe (a, Chunk s e a)
    uncons (Chunk n xs p) =
        let r = parse p xs in
        case r of
            Left  _       -> Nothing
            Right (x, ys) -> Just (x, Chunk (succ n) ys p)

instance HasOffset (Chunk s e a) where
    {-# INLINE offset #-}
    offset :: Chunk s e a -> Word
    offset (Chunk n _ _) = n


{- | Initialize a t'Chunk'-ing stream. -}
{-# INLINE initialize #-}
initialize :: s -> Parser s e a -> Chunk s e a
initialize = Chunk 0
