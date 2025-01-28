{- |
Module: Trisagion.Getters.ParseError

Parser combinators relying on special error handling with the @'ParseError'@ type.
-}

module Trisagion.Getters.ParseError (
    -- * Type aliases.
    Parser,

    -- * Error types.
    ValidationError (..),

    -- * Handling t'ParseError'.
    throwParseError,
    onParseError,
    validate,

    -- * Parsers without errors.
    failIff,
    until,

    -- * Post-conditions.
    guardWith,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (until)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Typeable)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Except (MonadError (..))

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Types.ParseError (ParseError, makeParseErrorNoBacktrace, makeParseError, initial)
import Trisagion.Types.Result (Result (..))
import Trisagion.Get (Get, embed, handleError, lookAhead, many)


{- | Type alias for the @'Get' s ('ParseError' ('PositionOf' s) e) a@ parser monad. -}
type Parser s e a = Get s (ParseError (PositionOf s) e) a


{- | The @ValidationError@ error type.

Error raised on failed validations against a predicate.
-}
data ValidationError = ValidationError
    deriving stock (Eq, Show)


{- | Parser that throws @t'ParseError' ('PositionOf' s) e@ with specified error tag.

The state component is the current parser position and the backtrace is a @'Nothing'@ of type
@'Maybe' ('ParseError' ('PositionOf' s) 'Void')@.
-}
{-# INLINE throwParseError #-}
throwParseError :: HasPosition s => e -> Parser s e Void
throwParseError e = embed $ Error . flip makeParseErrorNoBacktrace e

{- | Parser that swallows a thrown error as a backtrace for a new error.

Combinator is useful to add contextual information, e. g. with a parser like:

@
aParser = do
    ...
    x <- p -- ^ Can throw here.
@

Re-written as:

@
aParser = onParseError e q
    where
        -- If q errors with b, capture the stream position and throw e with b as backtrace.
        q = do
            ...
            x <- p -- ^ Can throw here.
            ...
@
-}
{-# INLINE onParseError #-}
onParseError
    :: (HasPosition s, Show d, Eq d, Typeable d)
    => e                            -- ^ Error tag of new thrown error.
    -> Parser s d a                 -- ^ Parser to run.
    -> Parser s e a
onParseError e p = do
    s <- get
    handleError
        p
        (\ b -> throwError $ makeParseError b s e)

{- | Run the parser and return the result, validating it. -}
{-# INLINE validate #-}
validate
    :: HasPosition s
    => (a -> Either d b)            -- ^ Validator.
    -> Parser s e a                 -- ^ Parser to run.
    -> Parser s (Either e d) b
validate v p = do
    s <- get
    r <- first (fmap Left) p
    either
        (throwError . makeParseErrorNoBacktrace s . Right)
        pure
        (v r)

{- | The parser @failIff p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws a @'ParseError' ('PositionOf' s) 'Void'@ if @p@ succeeds.

note(s):

    * This parser can be used to implement the longest match rule -- see 'until'.
-}
{-# INLINE failIff #-}
failIff :: Parser s e a -> Parser s Void ()
failIff p =
    first absurd (lookAhead p) >>=
        either
            (const $ pure ())
            (const . second absurd $ throwError mempty)

{- | The parser @'until' end p@ runs @p@ zero or more times until @end@ succeeds. -}
{-# INLINE until #-}
until
    :: Parser s e b                 -- ^ Closing parser.
    -> Parser s e a                 -- ^ Parser to run.
    -> Get s Void [a]
until end p = many $ first initial (failIff end) *> p


{- | Guard a parser with a post-condition.

A typical use case is to ensure all input was consumed, e. g. @'guardWith' p (const eoi)@.
-}
{-# INLINE guardWith #-}
guardWith
    :: HasPosition s
    => Parser s e a                 -- ^ Parser to run.
    -> (a -> Get s Void Bool)       -- ^ Post-condition parser.
    -> Parser s (Either ValidationError e) a
guardWith p cond = do
    x <- first (fmap Right) p
    t <- first absurd (cond x)
    if t
        then pure x
        else second absurd $ throwParseError (Left ValidationError)
