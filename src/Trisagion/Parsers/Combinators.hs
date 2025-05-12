{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * Error parsers.
    validate,

    -- * Parsers without errors.
    maybeP,
    lookAhead,

    -- * 'Applicative' parsers.
    skip,
    before,
    after,
    between,
    zipA,
    zipWithA,
    repeatA,

    -- * 'Alternative' parsers.
    manyP,
    someP,
    choose,
    untilEnd,

    -- * List parsers.
    sepBy,
    sepBy1,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- Package.
import Trisagion.Parser ((:+:), Parser, eval, get, try)


{- | Run the parser and return the result, validating it. -}
{-# INLINEABLE validate #-}
validate
    :: (a -> d :+: b)                   -- ^ Validator.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s (d :+: e) b
validate v p = do
    x <- first Right p
    case v x of
        Left d  -> throwError $ Left d
        Right y -> pure y


{- | @'maybeP' p@ runs @p@ returning the result as a 'Just'. On error, backtrack and return 'Nothing'.

The difference with 'Control.Applicative.optional' from 'Control.Applicative.Alternative' is the
more precise type signature.
-}
{-# INLINE maybeP #-}
maybeP :: Parser s e a -> Parser s Void (Maybe a)
maybeP p = either (const Nothing) Just <$> try p

{- | Run the parser and return the result, but do not consume any input. -}
{-# INLINE lookAhead #-}
lookAhead :: Parser s e a -> Parser s Void (e :+: a)
lookAhead p = eval p <$> get


{- | Run the parser and discard the result. -}
{-# INLINE skip #-}
skip :: Parser s e a -> Parser s e ()
skip = ($> ())

{- | The parser @'before' b p@ runs @b@ and @p@ in succession, returning the result of @p@. -}
{-# INLINE before #-}
before
    :: Parser s e b                     -- ^ Opening parser.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s e a
before = (*>)

{- | The parser @'after' a p@ runs @p@ and @a@ in succession, returning the result of @p@. -}
{-# INLINE after #-}
after
    :: Parser s e b                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s e a
after = flip (<*)

{- | The parser @'between' o c p@ runs @o@, @p@ and @c@, returning the result of @p@. -}
{-# INLINE between #-}
between
    :: Parser s e b                     -- ^ Opening parser.
    -> Parser s e c                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run in-between.
    -> Parser s e a
between open close = before open . after close

{- | Sequence two actions and zip the results in a pair. -}
{-# INLINE zipA #-}
zipA :: Applicative m => m a -> m b -> m (a, b)
zipA = liftA2 (,)

{- | Sequence two actions and zip the results with a binary function. -}
{-# INLINE zipWithA #-}
zipWithA :: Applicative m => (a -> b -> c) -> m a -> m b -> m c
zipWithA = liftA2

{- | Run the action @n@ times and return the list of results. -}
{-# INLINEABLE repeatA #-}
repeatA :: Applicative m => Word -> m a -> m [a]
repeatA n p = go n
    where
        go 0 = pure []
        go m = (:) <$> p <*> go (pred m)


{- | Choose between alternatives.

Run the first action and if it fails run the second. Return the results as an @'Either'@.
-}
{-# INLINE choose #-}
choose :: Alternative m => m a -> m b -> m (a :+: b)
choose q p = (Left <$> q) <|> (Right <$> p)

{- | Run the parser zero or more times until it fails, returning the list of results.

The difference with @'Control.Applicative.many'@ from 'Control.Applicative.Alternative' is the more
precise type signature.

note(s):

  * The @'manyP' p@ parser can loop forever if fed a parser @p@ that does not throw an error and
  does not consume input, e.g. any parser with @'Void'@ in the error type or their polymorphic
  variants, like @'pure' x@, @'Control.Applicative.many' p@, etc.
-}
{-# INLINEABLE manyP #-}
manyP :: Parser s e a -> Parser s Void [a]
manyP p = go
    where
        go = do
            r <- try p
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | Run the parser one or more times and return the results as a @'NonEmpty'@.

The difference with @'Control.Applicative.some'@ from 'Control.Applicative.Alternative' is the more
precise type signature.
-}
{-# INLINEABLE someP #-}
someP :: Parser s e a -> Parser s e (NonEmpty a)
someP p = liftA2 (:|) p (first absurd $ manyP p)

{- | @'untilEnd' end p@ runs @p@ until @end@ succeeds, returning the results of @p@ and @end@. -}
{-# INLINEABLE untilEnd #-}
untilEnd :: (Monad m, Alternative m) => m a -> m a -> m (NonEmpty a)
untilEnd end p = go
    where
        go = do
            r <- choose end p
            case r of
                Left e  -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | The parser @'sepBy' sep p@ parses zero or more occurences of @p@ separated by @sep@. -}
{-# INLINEABLE sepBy #-}
sepBy :: Parser s e a -> Parser s e b -> Parser s Void [b]
sepBy sep p = do
    x <- try p
    case x of
        Left _  -> pure []
        Right y -> (y :) <$> manyP (before sep p)

{- | The parser @'sepBy' sep p@ parses one or more occurences of @p@ separated by @sep@. -}
{-# INLINEABLE sepBy1 #-}
sepBy1 :: Parser s e a -> Parser s e b -> Parser s e (NonEmpty b)
sepBy1 sep p = liftA2 (:|) p (first absurd $ sepBy sep p)
