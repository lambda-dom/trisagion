{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators. The module should be imported qualified because some of the exported
conflict with base.
-}

module Trisagion.Parsers.Combinators (
    -- * Validators.
    validate,

    -- * Parsers without errors.
    maybe,
    lookAhead,
    failIff,

    -- * 'Applicative' parsers.
    skip,
    before,
    after,
    between,
    zip,
    zipWith,
    repeat,
    sequence,

    -- * 'Alternative' parsers.
    many,
    some,
    skipMany,
    skipSome,
    choose,
    pick,
    until,
    untilEnd,
    manyTill,

    -- * List parsers.
    sepBy,
    sepBy1,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (maybe, repeat, sequence, until, zip, zipWith)

-- Base.
import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- Package.
import Trisagion.Parser ((:+:), Parser, eval, get, try, throw)


{- | Run the parser and return the result, validating it.

note(s):

  * See 'Trisagion.Parsers.ParseError.validate' in 'Trisagion.Parsers.ParseError' for a version
  more suited for dealing with 'Trisagion.Types.ParseError.ParseError' errors.
-}
{-# INLINE validate #-}
validate
    :: (a -> d :+: b)                   -- ^ Validator.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s (d :+: e) b
validate v p = do
    x <- first Right p
    case v x of
        Left d  -> throwError $ Left d
        Right y -> pure y


{- | @'maybe' p@ runs @p@ returning the result as a 'Just'. On error, backtrack and return 'Nothing'.

The difference with 'Control.Applicative.optional' from 'Control.Applicative.Alternative' is the
more precise type signature.
-}
{-# INLINE maybe #-}
maybe :: Parser s e a -> Parser s Void (Maybe a)
maybe p = either (const Nothing) Just <$> try p

{- | Run the parser and return the result, but do not consume any input. -}
{-# INLINE lookAhead #-}
lookAhead :: Parser s e a -> Parser s Void (e :+: a)
lookAhead p = eval p <$> get

{- | The parser @'failIff' p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws the monoid unit for @e@ if @p@ succeeds.

note(s):

  * This parser can be used to implement the longest match rule -- see 'until'.
-}
{-# INLINE failIff #-}
failIff :: Monoid e => Parser s e a -> Parser s e ()
failIff p = do
    r <- first absurd $ lookAhead p
    case r of
        Left  _ -> pure ()
        Right _ -> absurd <$> throw mempty


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

{- | Sequence two parsers and zip the results in a pair. -}
{-# INLINE zip #-}
zip :: Parser s e a -> Parser s e b -> Parser s e (a, b)
zip = liftA2 (,)

{- | Sequence two parsers and zip the results with a binary function. -}
{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> Parser s e a -> Parser s e b -> Parser s e c
zipWith = liftA2

{- | Run the parser @n@ times and return the list of results. -}
{-# INLINEABLE repeat #-}
repeat :: Word -> Parser s e a -> Parser s e [a]
repeat n p = go n
    where
        go 0 = pure []
        go m = (:) <$> p <*> go (pred m)

{- | Sequence a traversable of parsers and return the traversable of results. -}
{-# INLINE sequence #-}
sequence :: Traversable t => t (Parser s e a) -> Parser s e (t a)
sequence = sequenceA


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
  does not consume input, e.g. any parser with @'Void'@ in the error type or their polymorphic
  variants, like @'pure' x@, @'Control.Applicative.many' p@, etc.
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

{- | Run the parser one or more times until it fails and return the results as a @'NonEmpty'@.

The difference with @'Control.Applicative.some'@ from 'Control.Applicative.Alternative' is the more
precise type signature.
-}
{-# INLINE some #-}
some :: Parser s e a -> Parser s e (NonEmpty a)
some p = liftA2 (:|) p (first absurd $ many p)

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

{- | The parser @'until' end p@ runs @p@ zero or more times until @end@ succeeds.

note(s):

  * The difference with 'manyTill' is that the @end@ parser will not consume any input.
-}
{-# INLINE until #-}
until
    :: Monoid e
    => Parser s e b                   -- ^ Closing parser.
    -> Parser s e a                   -- ^ Parser to run.
    -> Parser s Void [a]
until end p = many $ failIff end *> p

{- | @'untilEnd' end p@ runs @p@ until @end@ succeeds, returning the results of @p@ and @end@. -}
{-# INLINEABLE untilEnd #-}
untilEnd :: Monoid e => Parser s e a -> Parser s e a -> Parser s e (NonEmpty a)
untilEnd end p = go
    where
        go = do
            r <- choose end p
            case r of
                Left e  -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | @'manyTill' end p@ runs @p@ until @end@ succeeds, returning the results of @p@. -}
{-# INLINEABLE manyTill #-}
manyTill :: Monoid e => Parser s e a -> Parser s e b -> Parser s e [b]
manyTill end p = go
    where
        go = do
            r <- choose end p
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | The parser @'sepBy' sep p@ parses zero or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy #-}
sepBy :: Parser s e a -> Parser s e b -> Parser s Void [b]
sepBy sep p = do
    x <- try p
    case x of
        Left _  -> pure []
        Right y -> (y :) <$> many (before sep p)

{- | The parser @'sepBy1' sep p@ parses one or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy1 #-}
sepBy1 :: Parser s e a -> Parser s e b -> Parser s e (NonEmpty b)
sepBy1 sep p = liftA2 (:|) p (first absurd $ sepBy sep p)
