{- |
Module: Trisagion.Getters.Combinators

General parser combinators.
-}

module Trisagion.Getters.Combinators (
    -- * Parsers without errors.
    observe,
    lookAhead,
    maybe,

    -- * Handling t'ParseError'.
    validate,
    backtrackParseError,
    onParseError,

    -- * Parsers without values.
    failIff,
    skip,

    -- * Applicative parsers.
    zip,
    zipWith,
    before,
    after,
    between,

    -- * Alternative parsers.
    eitherOf,
    choose,

    -- * List parsers.
    sequence,
    repeat,
    unfold,
    many,
    some,
    atMostN,
    until,
    untilEnd,
    sepBy,
    sepBy1,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (maybe, repeat, sequence, until, zip, zipWith)

-- Base.
import Control.Applicative (Alternative ((<|>)), asum)
import Control.Monad (replicateM)
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Typeable)
import Data.Foldable (Foldable (..))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Types.ParseError (ParseError (..), initial, makeParseError, mapWith)
import Trisagion.Get (Get, handleError, eval)


{- | Run the parser and return the result as a @Right@; if it errors, backtrack and return the error as a @Left@. -}
observe :: Get s e a -> Get s Void (Either e a)
observe p = do
    s <- get
    handleError
        (Right <$> p)
        (\ e -> put s $> Left e)

{- | Run the parser and return the result, but do not consume any input. -}
lookAhead :: Get s e a -> Get s Void (Either e a)
lookAhead p = eval p <$> first absurd get

{- | Run the parser and return the result as a @'Just'@. If it errors, backtrack and return @'Nothing'@.

The difference with @'Control.Applicative.optional'@ is the more precise type signature.
-}
maybe :: Get s e a -> Get s Void (Maybe a)
maybe p = either (const Nothing) Just <$> observe p

{- | Run parser and return the result, validating it. -}
validate
    :: (a -> Either d b)            -- ^ Validator.
    -> Get s (ParseError s e) a     -- ^ Parser to run.
    -> Get s (ParseError s (Either e d)) b
validate v p = do
    s <- get
    r <- first (fmap Left) p
    either
        (throwError . makeParseError s . Right)
        pure
        (v r)

{- | Backtrack the parser state component of a thrown 'ParseError'. -}
backtrackParseError
    :: Get s (ParseError s e) a
    -> Get s (ParseError s e) a
backtrackParseError p = do
        s <- get
        handleError
            p
            (throwError . mapWith id (const s) id)

{- | Add error context to a 'ParseError'. -}
onParseError
    :: (Show d, Eq d, Typeable d)
    => e                            -- ^ Error tag for contextual error.
    -> Get s (ParseError s d) a     -- ^ Parser to try.
    -> Get s (ParseError s e) a
onParseError err p = do
    s <- get
    handleError
        p
        (\ d -> throwError $ ParseError (Just d) s err)

{- | The parser @failIff p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws a @t'ParseError' s 'Void'@ if @p@ succeeds.

note(s):

    * This parser can be used to implement the longest match rule -- see 'until' below.
-}
failIff :: Get s e a -> Get s (ParseError s Void) ()
failIff p =
    first absurd (lookAhead p) >>=
        either
            (const $ pure ())
            (const . second absurd $ throwError mempty)

{- | Run the parser but discard the result.

note(s):

    * Various combinators have more efficient @skip*@ versions that avoid constructing intermediate
    values; use those whenever possible.
-}
skip :: Get s e a -> Get s e ()
skip = (() <$)

{- | Sequence two parsers and zip the results in a pair. -}
zip :: Get s e a -> Get s e b -> Get s e (a, b)
zip = zipWith (,)

{- | Sequence two parsers and zip the results with a binary function. -}
zipWith :: (a -> b -> c) -> Get s e a -> Get s e b -> Get s e c
zipWith f p q = f <$> p <*> q

{- | The parser @'before' b p@ parses @b@ and @p@ in succession, returning the result of @p@. -}
before
    :: Get s e b                -- ^ Parser to run first.
    -> Get s e a                -- ^ Parser to run second.
    -> Get s e a
before b p = b *> p

{- | The parser @'after' a p@ parses @p@ and @a@ in succession, returning the result of @p@. -}
after
    :: Get s e b                -- ^ Parser to run after.
    -> Get s e a                -- ^ Parser to run first.
    -> Get s e a
after a p = p <* a

{- | The parser @'between' o c p@ parses @o@, @p@ and @c@ in succession, returning the result of @p@. -}
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
eitherOf :: Monoid e => Get s e a -> Get s e b -> Get s e (Either a b)
eitherOf q p = (Left <$> q) <|> (Right <$> p)

{- | Run the parsers in succession, returning the result of the first successful one. -}
choose :: (Foldable t, Monoid e) => t (Get s e a) -> Get s e a
choose = asum

{- | Sequence a traversable of parsers and return the traversable of results. -}
sequence :: Traversable t => t (Get s e a) -> Get s e (t a)
sequence = sequenceA

{- | Run the parser @n@ times and return the list of results.

It is guaranteed that the list of results has exactly @n@ elements.
-}
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

{- | The parser @'until' end p@ runs @p@ zero or more times until @end@ succeeds. -}
until
    :: Monoid e
    => Get s e b                -- ^ Closing parser.
    -> Get s e a                -- ^ Parser to run.
    -> Get s Void [a]
until end p = many $ first initial (failIff end) *> p

{- | The parser @'untilEnd' end p@ runs @p@ zero or more times until @end@ succeeds, returning the results of @p@ and @end@. -}
untilEnd :: Monoid e => Get s e a -> Get s e a -> Get s e (NonEmpty a)
untilEnd end p = go
    where
        go = do
            r <- eitherOf end p
            case r of
                Left e -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | The parser @'sepBy sep p'@ parses zero or more occurences of @p@ separated by @sep@. -}
sepBy :: Get s e a -> Get s e b -> Get s Void [b]
sepBy sep p = fromMaybe [] <$> maybe (toList <$> sepBy1 sep p)

{- | The parser @'sepBy sep p'@ parses one or more occurences of @p@ separated by @sep@. -}
sepBy1 :: Get s e a -> Get s e b -> Get s e (NonEmpty b)
sepBy1 sep p = zipWith (:|) p (first absurd $ many (sep *> p))
