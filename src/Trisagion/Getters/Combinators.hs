{- |
Module: Trisagion.Getters.Combinators

General parser combinators.
-}

module Trisagion.Getters.Combinators (
    -- * Parsers without errors.
    observe,
    lookAhead,
    option,

    -- * State parsers.
    replace,

    -- * Handling t'ParseError'.
    validate,
    onParseErrorWith,
    onParseError,

    -- * Parsers without values.
    failIff,
    skip,

    -- * Applicative parsers.
    pair,
    pairWith,
    between,

    -- * Alternative parsers.
    eitherOf,
    choose,

    -- * List parsers.
    chain,
    repeatN,
    unfold,
    repeatedly,
    atMostN,
    atLeastOne,
    manyTill,
    manyTill_,
    sepBy,
    sepBy1,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..), asum)
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
import Control.Monad.State (MonadState (..), gets)

-- Package.
import Trisagion.Types.ParseError (ParseError (..), initial, makeParseError)
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
option :: Get s e a -> Get s Void (Maybe a)
option p = either (const Nothing) Just <$> observe p

{- | Parser returning the remainder of the state and replacing it with something else. -}
replace :: (s -> s) -> Get s Void s
replace f = get <* (gets f >>= put) 

{- | Run parser and return the result, validating it. -}
validate
    :: (a -> Either d b)            -- ^ Validator.
    -> Get s (ParseError s e) a     -- ^ Parser to run and validate.
    -> Get s (ParseError s (Either e d)) b
validate v p = do
    s <- get
    r <- first (fmap Left) p
    either
        (throwError . makeParseError s . Right)
        pure
        (v r)

{- | Add error context to a parser. -}
onParseErrorWith
    :: (Show d, Eq d, Typeable d)
    => s                            -- ^ State component of the error to throw.
    -> e                            -- ^ Error tag for contextual error.
    -> Get s (ParseError s d) a     -- ^ Parser to try.
    -> Get s (ParseError s e) a
onParseErrorWith s err p = do
    handleError
        p
        (\ e -> throwError $ ParseError (Just e) s err)

{- | Specialized version of 'onParseErrorWith' capturing the current parser state. -}
onParseError
    :: (Show d, Eq d, Typeable d)
    => e                            -- ^ Error tag for contextual error.
    -> Get s (ParseError s d) a     -- ^ Parser to try.
    -> Get s (ParseError s e) a
onParseError err p = do
    s <- get
    onParseErrorWith s err p

{- | The parser @failIff p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws a @t'ParseError' s 'Void'@ if @p@ succeeds.

note(s):

    * This parser can be used to implement the longest match rule -- see 'manyTill' below.
-}
failIff :: Get s e a -> Get s (ParseError s Void) ()
failIff p = do
    r <- first absurd (lookAhead p)
    case r of
        Left _  -> pure ()
        Right _ -> second absurd $ throwError mempty

{- | Run the parser but discard the result.

note(s):

    * Various combinators have more efficient @skip*@ versions that avoid constructing intermediate
    values; use those whenever possible.
-}
skip :: Get s e a -> Get s e ()
skip = (() <$)

{- | Sequence two parsers and zip the results in a pair. -}
pair :: Get s e a -> Get s e b -> Get s e (a, b)
pair = pairWith (,)

{- | Sequence two parsers and zip the results with a binary function. -}
pairWith :: (a -> b -> c) -> Get s e a -> Get s e b -> Get s e c
pairWith f p q = f <$> p <*> q

{- | The parser @'between' o c p@ parses @o@, @p@ and @c@ in succession, returning the result of @p@. -}
between
    :: Get s e b                -- ^ Opening parser.
    -> Get s e c                -- ^ Closing parser.
    -> Get s e a                -- ^ Parser to run in-between.
    -> Get s e a
between open close p = open *> p <* close

{- | Run the first parser and if it fails run the second. Return the result as an @'Either'@.

note(s):

    * The parser is @'Left'@-biased; if the first parser is successful the second never runs.
-}
eitherOf :: Monoid e => Get s e a -> Get s e b -> Get s e (Either a b)
eitherOf q p = (Left <$> q) <|> (Right <$> p)

{- | Run the parsers in succession, returning the result of the first successful one. -}
choose :: (Foldable t, Monoid e) => t (Get s e a) -> Get s e a
choose = asum

{- | Chain together a traversable of parsers and return the results. -}
chain :: Traversable t => t (Get s e a) -> Get s e (t a)
chain = sequenceA

{- | Run the parser @n@ times and return the list of results.

It is guaranteed that the list of results has exactly @n@ elements.
-}
repeatN :: Word -> Get s e a -> Get s e [a]
repeatN = replicateM . fromIntegral

{- | Lift @'List.unfoldr'@ over the @t'Get'@ monad. -}
unfold :: (r -> Get s e (a, r)) -> r -> Get s Void [a]
unfold h = go
    where
        go s = do
            r <- option (h s)
            case r of
                Nothing     -> pure []
                Just (x, t) -> (x : ) <$> go t

{- | Repeatedly run the parser until it fails, returning the list of results.

The difference with @'many'@ from @'Alternative'@ is the more precise type signature.

note(s):

    * The @'repeatedly' p@ parser can loop forever if fed a parser @p@ that does not fail and that
    may not consume input, e.g. any parser with @'Void'@ in the error type or their polymorphic
    variants, like @'pure' x@, @'many' p@, etc.
-}
repeatedly :: Get s e a -> Get s Void [a]
repeatedly p = go
    where
        go = do
            r <- option p
            case r of
                Nothing -> pure []
                Just x  -> (x :) <$> go

{- | Run the parser @n@ times or until it errors and return the list of results.

The parser does not error and it is guaranteed that the list of results has @n@ or less elements.
-}
atMostN :: Word -> Get s e a -> Get s Void [a]
atMostN n p = go n
    where
        go 0 = pure []
        go i = do
            r <- option p
            case r of
                Nothing -> pure []
                Just x  -> (x :) <$> go (i - 1)

{- | Run the parser one or more times and return the results as a @'NonEmpty'@.

The difference with @'some'@ from 'Alternative' is the more precise type signature.
-}
atLeastOne :: Get s e a -> Get s e (NonEmpty a)
atLeastOne p = pairWith (:|) p (first absurd $ repeatedly p)

{- | The parser @'manyTill' end p@ runs @p@ zero or more times until @end@ succeeds. -}
manyTill
    :: Monoid e
    => Get s e b                -- ^ Closing parser.
    -> Get s e a                -- ^ Parser to run.
    -> Get s Void [a]
manyTill end p = repeatedly $ first initial (failIff end) *> p

{- | The parser @'manyTill_' end p@ runs @p@ zero or more times until @end@ succeeds, returning the results of @p@ and @end@. -}
manyTill_ :: Monoid e => Get s e a -> Get s e a -> Get s e (NonEmpty a)
manyTill_ end p = go
    where
        go = do
            r <- eitherOf end p
            case r of
                Left e -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | The parser @'sepBy sep p'@ parses zero or more occurences of @p@ separated by @sep@. -}
sepBy :: Get s e a -> Get s e b -> Get s Void [b]
sepBy sep p = fromMaybe [] <$> option (toList <$> sepBy1 sep p)

{- | The parser @'sepBy sep p'@ parses one or more occurences of @p@ separated by @sep@. -}
sepBy1 :: Get s e a -> Get s e b -> Get s e (NonEmpty b)
sepBy1 sep p = pairWith (:|) p (first absurd $ repeatedly (sep *> p))
