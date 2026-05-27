{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * Parsers without errors.
    optional,
    failIff,

    -- * 'Applicative' parsers.
    skip,
    between,
    pair,
    pairWith,
    count,
    chain,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Package.
import Trisagion.Parser (Parser, try, lookAhead, throw)


{- | @'optional' p@ runs @p@ returning the result as a 'Just'. On error, backtrack and return 'Nothing'.

The difference with 'Control.Applicative.optional' from 'Control.Applicative.Alternative' is the
more precise type signature.
-}
{-# INLINE optional #-}
optional :: Parser s e a -> Parser s Void (Maybe a)
optional p = either (const Nothing) Just <$> try p

{- | The parser @'failIff' p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws the monoid unit for @e@ if @p@ succeeds.
-}
{-# INLINE failIff #-}
failIff :: Monoid e => Parser s e a -> Parser s e ()
failIff p = do
    r <- first absurd $ lookAhead p
    case r of
        Left  _ -> pure ()
        Right _ -> throw mempty


{- | Run the parser and discard the result. -}
{-# INLINE skip #-}
skip :: Parser s e a -> Parser s e ()
skip = ($> ())

{- | The parser @'between' o c p@ runs @o@, @p@ and @c@, returning the result of @p@. -}
{-# INLINE between #-}
between
    :: Parser s e b                     -- ^ Opening parser.
    -> Parser s e c                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run in-between.
    -> Parser s e a
between open close p = open *> p <* close

{- | Sequence two parsers and pair up the results. -}
{-# INLINE pair #-}
pair :: Parser s e a -> Parser s e b -> Parser s e (a, b)
pair = pairWith (,)

{- | Sequence two parsers and pair the results with a binary function. -}
{-# INLINE pairWith #-}
pairWith :: (a -> b -> c) -> Parser s e a -> Parser s e b -> Parser s e c
pairWith = liftA2

{- | Run the parser @n@ times and return the list of results. -}
{-# INLINEABLE count #-}
count :: Word -> Parser s e a -> Parser s e [a]
count n p = go n
    where
        go 0 = pure []
        go m = (:) <$> p <*> go (pred m)

{- | Chain together a traversable of parsers and return the traversable of results. -}
{-# INLINE chain #-}
chain :: Traversable t => t (Parser s e a) -> Parser s e (t a)
chain = sequenceA

