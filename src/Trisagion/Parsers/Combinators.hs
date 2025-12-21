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
    some,
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
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError(..))

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.ParserT (ParserT, try, lookAhead, mapError)


{- | @'optional' p@ runs @p@ returning the result as a 'Just'. On error, backtrack and return 'Nothing'.

The difference with 'Control.Applicative.optional' from 'Control.Applicative.Alternative' is the
more precise type signature.
-}
{-# INLINE optional #-}
optional :: Monad m => ParserT s e m a -> ParserT s Void m (Maybe a)
optional p = either (const Nothing) Just <$> try p

{- | The parser @'failIff' p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws the monoid unit for @e@ if @p@ succeeds.
-}
{-# INLINE failIff #-}
failIff :: (Monad m, Monoid e) => ParserT s e m a -> ParserT s e m ()
failIff p = do
    r <- mapError absurd $ lookAhead p
    case r of
        Left  _ -> pure ()
        Right _ -> throwError mempty

{- | Run the parser and discard the result. -}
{-# INLINE skip #-}
skip :: Functor m => ParserT s e m a -> ParserT s e m ()
skip = ($> ())

{- | The parser @'between' o c p@ runs @o@, @p@ and @c@, returning the result of @p@. -}
{-# INLINE between #-}
between
    :: Monad m
    => ParserT s e m b                  -- ^ Opening parser.
    -> ParserT s e m c                  -- ^ Closing parser.
    -> ParserT s e m a                  -- ^ Parser to run in-between.
    -> ParserT s e m a
between open close p = open *> p <* close

{- | Sequence two parsers and pair up the results. -}
{-# INLINE pair #-}
pair :: Monad m => ParserT s e m a -> ParserT s e m b -> ParserT s e m (a, b)
pair = pairWith (,)

{- | Sequence two parsers and pair the results with a binary function. -}
{-# INLINE pairWith #-}
pairWith :: Monad m => (a -> b -> c) -> ParserT s e m a -> ParserT s e m b -> ParserT s e m c
pairWith = liftA2

{- | Run the parser @n@ times and return the list of results. -}
{-# INLINEABLE count #-}
count :: Monad m => Word -> ParserT s e m a -> ParserT s e m [a]
count n p = go n
    where
        go 0 = pure []
        go m = (:) <$> p <*> go (pred m)

{- | Chain together a traversable of parsers and return the traversable of results. -}
{-# INLINE chain #-}
chain :: (Monad m, Traversable t) => t (ParserT s e m a) -> ParserT s e m (t a)
chain = sequenceA

{- | Choose between two parsers.

Run the first parser and if it fails run the second. Return the results as an @'Either'@.
-}
{-# INLINE choose #-}
choose :: (Monad m, Monoid e) => ParserT s e m a -> ParserT s e m b -> ParserT s e m (a :+: b)
choose q p = (Left <$> q) <|> (Right <$> p)

{- | Pick between a foldable of parsers.

Run the parsers in succession returning the result of the first successful one.
-}
{-# INLINE pick #-}
pick :: (Monad m, Foldable t, Monoid e) => t (ParserT s e m a) -> ParserT s e m a
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
many :: Monad m => ParserT s e m a -> ParserT s Void m [a]
many p = go
    where
        go = do
            r <- try p
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | Run the parser one or more times until it fails and return the results as a @'NonEmpty'@.

The difference with @'Control.Applicative.some'@ from 'Control.Applicative.Alternative' is the more
precise type signature.
-}
{-# INLINE some #-}
some :: Monad m => ParserT s e m a -> ParserT s e m (NonEmpty a)
some p = pairWith (:|) p (mapError absurd $ many p)

{- | Run the parser zero or more times until it fails and discard the results. -}
{-# INLINEABLE skipMany #-}
skipMany :: Monad m => ParserT s e m a -> ParserT s Void m ()
skipMany p = go
    where
        go = do
            r <- try p
            case r of
                Left _  -> pure ()
                Right _ -> go

{- | Run the parser one or more times until it fails and discard the results. -}
{-# INLINE skipSome #-}
skipSome :: Monad m => ParserT s e m a -> ParserT s e m ()
skipSome p = p *> mapError absurd (skipMany p)

{- | The parser @'untilEnd' end p@ runs @p@ zero or more times until @end@ succeeds.

note(s):

  * The difference with 'manyTill' is that the @end@ parser will not consume any input.
-}
{-# INLINE untilEnd #-}
untilEnd
    :: (Monad m, Monoid e)
    => ParserT s e m b                  -- ^ Closing parser.
    -> ParserT s e m a                  -- ^ Parser to run.
    -> ParserT s Void m [a]
untilEnd end p = many $ failIff end *> p

{- | @'manyTill' end p@ runs @p@ until @end@ succeeds, returning the results of @p@. -}
{-# INLINEABLE manyTill #-}
manyTill :: (Monad m, Monoid e) => ParserT s e m a -> ParserT s e m b -> ParserT s e m [b]
manyTill end p = go
    where
        go = do
            r <- choose end p
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | @'manyTillEnd' end p@ runs @p@ until @end@ succeeds, returning the results of @p@ and @end@. -}
{-# INLINEABLE manyTillEnd #-}
manyTillEnd
    :: (Monad m, Monoid e)
    => ParserT s e m a                  -- ^ Closing parser.
    -> ParserT s e m a                  -- ^ Parser to run.
    -> ParserT s e m (NonEmpty a)
manyTillEnd end p = go
    where
        go = do
            r <- choose end p
            case r of
                Left e  -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | The parser @'sepBy' sep p@ parses zero or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy #-}
sepBy :: Monad m => ParserT s e m a -> ParserT s e m b -> ParserT s Void m [b]
sepBy sep p = do
    x <- try p
    case x of
        Left _  -> pure []
        Right y -> (y :) <$> many (sep *> p)

{- | The parser @'sepBy1' sep p@ parses one or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy1 #-}
sepBy1 :: Monad m => ParserT s e m a -> ParserT s e m b -> ParserT s e m (NonEmpty b)
sepBy1 sep p = liftA2 (:|) p (mapError absurd $ many (sep *> p))
