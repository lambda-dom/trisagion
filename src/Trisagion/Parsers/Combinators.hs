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

