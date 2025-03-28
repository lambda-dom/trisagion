{- |
Module: Trisagion.Streams.Chunk

Chunk a stream with a parser.
-}

module Trisagion.Streams.Chunk (
    -- * Streams.
    Chunk (..),
) where

-- Imports.
-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Types.Result (toEither)
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Parser (Parser, run)


{- | Chunk a streamable with a parser. -}
data Chunk s e a where
    Chunk :: s -> Parser s e a -> Chunk s e a


-- Instances.
instance MonoFunctor (Chunk s e a) where
    {- | The type of the elemnts of a chunked stream. -}
    type ElementOf (Chunk s e a) = a

    {- | Monomorphic map over a chunked stream. -}
    monomap :: (a -> a) -> Chunk s e a -> Chunk s e a
    monomap f (Chunk stream parser) = Chunk stream (f <$> parser)

instance Streamable (Chunk s e a) where
    splitOne ::Chunk s e a -> Maybe (a, Chunk s e a)
    splitOne (Chunk s p) = either (const Nothing) (Just . fmap (`Chunk` p)) . toEither $ run p s
