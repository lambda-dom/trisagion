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

    -- * 'Alternative' parsers.
    choose,
    pick,
    many,
    skipMany,
    skipSome,
    untilEnd,
    manyTill,
    manyTillEnd,
    sepBy,
    sepBy1,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative ((<|>)), asum)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parser (Parser, try, lookAhead)
import Data.List.NonEmpty (NonEmpty (..), (<|))


{- | @'optional' p@ runs @p@ returning the result as a 'Just'. On error, backtrack and return 'Nothing'.

The difference with 'Control.Applicative.optional' from 'Control.Applicative.Alternative' is the
more precise type signature.

=== __Examples:__

>>> parse (optional $ matchOne '0') "0123"

>>> parse (optional $ matchOne '1') "0123"

>>> parse (optional $ matchOne '0') ""
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
        Right _ -> throwError mempty


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


{- | Choose between two parsers.

Run the first parser and if it fails run the second. Return the results as an @'Either'@.
-}
{-# INLINE choose #-}
choose :: Monoid e => Parser s e a -> Parser s e b -> Parser s e (a :+: b)
choose q p = (Left <$> q) <|> (Right <$> p)

{- | Pick between a foldable of parsers.

Run the parsers in succession returning the result of the first successful one.
-}
{-# INLINE pick #-}
pick :: (Foldable t, Monoid e) => t (Parser s e a) -> Parser s e a
pick = asum

{- | Run the parser zero or more times until it fails, returning the list of results.

The difference with @'Control.Applicative.many'@ from 'Control.Applicative.Alternative' is the more
precise type signature.

note(s):

  * The @'many' p@ parser can loop forever if fed a parser @p@ that does not throw an error and
  possibly does not consume input, e.g. any parser with @'Void'@ in the error type or their
  polymorphic variants, like @'pure' x@, @'Control.Applicative.many' p@, etc.
-}
{-# INLINEABLE many #-}
many :: Parser s e a -> Parser s Void [a]
many p = go
    where
        go = do
            r <- try p
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | Run the parser zero or more times until it fails and discard the results. -}
{-# INLINEABLE skipMany #-}
skipMany :: Parser s e a -> Parser s Void ()
skipMany p = go
    where
        go = do
            r <- try p
            case r of
                Left _  -> pure ()
                Right _ -> go

{- | Run the parser one or more times until it fails and discard the results. -}
{-# INLINE skipSome #-}
skipSome :: Parser s e a -> Parser s e ()
skipSome p = p *> first absurd (skipMany p)

{- | The parser @'untilEnd' end p@ runs @p@ zero or more times until @end@ succeeds.

note(s):

  * The difference with 'manyTill' is the need for the monoid constraint and the fact that the
  @end@ parser will not consume any input.
-}
{-# INLINE untilEnd #-}
untilEnd
    :: Monoid e
    => Parser s e b                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s Void [a]
untilEnd end p = many $ failIff end *> p

{- | @'manyTill' end p@ runs @p@ until @end@ succeeds, returning the results of @p@. -}
{-# INLINEABLE manyTill #-}
manyTill :: Parser s e a -> Parser s e b -> Parser s e [b]
manyTill end p = go
    where
        go = do
            r <- catchError (fmap Left end) (const (fmap Right p))
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | @'manyTillEnd' end p@ runs @p@ until @end@ succeeds, returning the results of @p@ and @end@. -}
{-# INLINEABLE manyTillEnd #-}
manyTillEnd
    :: Parser s e a                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s e (NonEmpty a)
manyTillEnd end p = go
    where
        go = do
            r <- catchError (fmap Left end) (const (fmap Right p))
            case r of
                Left e  -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | The parser @'sepBy' sep p@ parses zero or more occurrences of @p@ separated by @sep@. -}
{-# INLINE sepBy #-}
sepBy :: Parser s e a -> Parser s e b -> Parser s Void [b]
sepBy sep p = do
    x <- try p
    case x of
        Left _  -> pure []
        Right y -> (y :) <$> many (sep *> p)

{- | The parser @'sepBy1' sep p@ parses one or more occurrences of @p@ separated by @sep@. -}
{-# INLINE sepBy1 #-}
sepBy1 :: Parser s e a -> Parser s e b -> Parser s e (NonEmpty b)
sepBy1 sep p = liftA2 (:|) p (first absurd $ many (sep *> p))
