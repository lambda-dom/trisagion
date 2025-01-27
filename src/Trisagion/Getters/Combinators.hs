{- |
Module: Trisagion.Getters.Combinators

General parser combinators that do not rely on special error handling with @ParseError@.
-}

module Trisagion.Getters.Combinators (
    -- * Parsers without errors.
    observe,
    lookAhead,
    maybe,

    -- * Parsers without values.
    skip,

    -- * Applicative parsers.
    zip,
    zipWith,
    before,
    after,
    between,

    -- * Alternative parsers.
    either,
    choose,

    -- * List parsers.
    sequence,
    repeat,
    unfold,
    many,
    some,
    atMostN,
    untilEnd,
    sepBy,
    sepBy1,
) where

-- Imports.
-- Prelude hiding.
import Prelude qualified as Base (either)
import Prelude hiding (either, maybe, repeat, sequence, zipWith, zip)

-- Base.
import Control.Applicative (Alternative ((<|>)), asum)
import Control.Monad (replicateM)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..), (<|), toList)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Get (Get, handleError, eval)


{- | Run the parser and return the result as a @Right@; if it errors, backtrack and return the error as a @Left@. -}
{-# INLINE observe #-}
observe :: Get s e a -> Get s Void (Either e a)
observe p = do
    s <- get
    handleError
        (Right <$> p)
        (\ e -> put s $> Left e)

{- | Run the parser and return the result, but do not consume any input. -}
{-# INLINE lookAhead #-}
lookAhead :: Get s e a -> Get s Void (Either e a)
lookAhead p = eval p <$> first absurd get

{- | Run the parser and return the result as a @'Just'@. If it errors, backtrack and return @'Nothing'@.

The difference with @'Control.Applicative.optional'@ is the more precise type signature.
-}
{-# INLINE maybe #-}
maybe :: Get s e a -> Get s Void (Maybe a)
maybe p = Base.either (const Nothing) Just <$> observe p

{- | Run the parser but discard the result.

note(s):

    * Various combinators have more efficient @skip*@ versions that avoid constructing intermediate
    values; use those whenever possible.
-}
{-# INLINE skip #-}
skip :: Get s e a -> Get s e ()
skip = (() <$)

{- | Sequence two parsers and zip the results in a pair. -}
{-# INLINE zip #-}
zip :: Get s e a -> Get s e b -> Get s e (a, b)
zip = zipWith (,)

{- | Sequence two parsers and zip the results with a binary function. -}
{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> Get s e a -> Get s e b -> Get s e c
zipWith f p q = f <$> p <*> q

{- | The parser @'before' b p@ parses @b@ and @p@ in succession, returning the result of @p@. -}
{-# INLINE before #-}
before
    :: Get s e b                -- ^ Parser to run first.
    -> Get s e a                -- ^ Parser to run second.
    -> Get s e a
before b p = b *> p

{- | The parser @'after' a p@ parses @p@ and @a@ in succession, returning the result of @p@. -}
{-# INLINE after #-}
after
    :: Get s e b                -- ^ Parser to run after.
    -> Get s e a                -- ^ Parser to run first.
    -> Get s e a
after a p = p <* a

{- | The parser @'between' o c p@ parses @o@, @p@ and @c@ in succession, returning the result of @p@. -}
{-# INLINE between #-}
between
    :: Get s e b                -- ^ Opening parser.
    -> Get s e c                -- ^ Closing parser.
    -> Get s e a                -- ^ Parser to run in-between.
    -> Get s e a
between open close = before open . after close

{- | Run the first parser and if it fails run the second. Return the result as an @'Either'@.

note(s):

    * The parser is @'Left'@-biased; if the first parser is successful the second never runs.
-}
{-# INLINE either #-}
either :: Monoid e => Get s e a -> Get s e b -> Get s e (Either a b)
either q p = (Left <$> q) <|> (Right <$> p)

{- | Run the parsers in succession, returning the result of the first successful one. -}
{-# INLINE choose #-}
choose :: (Foldable t, Monoid e) => t (Get s e a) -> Get s e a
choose = asum

{- | Sequence a traversable of parsers and return the traversable of results. -}
{-# INLINE sequence #-}
sequence :: Traversable t => t (Get s e a) -> Get s e (t a)
sequence = sequenceA

{- | Run the parser @n@ times and return the list of results.

It is guaranteed that the list of results has exactly @n@ elements.
-}
{-# INLINE repeat #-}
repeat :: Word -> Get s e a -> Get s e [a]
repeat = replicateM . fromIntegral

{- | Lift @'List.unfoldr'@ over the @t'Get'@ monad. -}
unfold :: (r -> Get s e (a, r)) -> r -> Get s Void [a]
unfold h = go
    where
        go s = do
            r <- maybe (h s)
            case r of
                Nothing     -> pure []
                Just (x, t) -> (x : ) <$> go t

{- | Run the parser zero or more times until it fails, returning the list of results.

The difference with @'Control.Applicative.many'@ is the more precise type signature.

note(s):

    * The @'many' p@ parser can loop forever if fed a parser @p@ that does not fail and that
    may not consume input, e.g. any parser with @'Void'@ in the error type or their polymorphic
    variants, like @'pure' x@, @'Control.Applicative.many' p@, etc.
-}
many :: Get s e a -> Get s Void [a]
many p = go
    where
        go = do
            r <- maybe p
            case r of
                Nothing -> pure []
                Just x  -> (x :) <$> go

{- | Run the parser one or more times and return the results as a @'NonEmpty'@.

The difference with @'Control.Applicative.some'@ is the more precise type signature.
-}
{-# INLINE some #-}
some :: Get s e a -> Get s e (NonEmpty a)
some p = zipWith (:|) p (first absurd $ many p)

{- | Run the parser @n@ times or until it errors and return the list of results.

The parser does not error and it is guaranteed that the list of results has @n@ or less elements.
-}
atMostN :: Word -> Get s e a -> Get s Void [a]
atMostN n p = go n
    where
        go 0 = pure []
        go i = do
            r <- maybe p
            case r of
                Nothing -> pure []
                Just x  -> (x :) <$> go (pred i)

{- | The parser @'untilEnd' end p@ runs @p@ zero or more times until @end@ succeeds, returning the results of @p@ and @end@. -}
untilEnd :: Monoid e => Get s e a -> Get s e a -> Get s e (NonEmpty a)
untilEnd end p = go
    where
        go = do
            r <- either end p
            case r of
                Left e -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | The parser @'sepBy' sep p@ parses zero or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy #-}
sepBy :: Get s e a -> Get s e b -> Get s Void [b]
sepBy sep p = fromMaybe [] <$> maybe (toList <$> sepBy1 sep p)

{- | The parser @'sepBy' sep p@ parses one or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy1 #-}
sepBy1 :: Get s e a -> Get s e b -> Get s e (NonEmpty b)
sepBy1 sep p = zipWith (:|) p (first absurd $ many (sep *> p))
