{- |
Module: Trisagion.Getters.ParseError

Parser combinators relying on special error handling with the @'ParseError'@ type.
-}

module Trisagion.Getters.ParseError (
    -- * Error types.
    ValidationError (..),

    -- * Handling t'ParseError'.
    throwParseError,
    withPosition,
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
import Trisagion.Types.ParseError (ParseError, makeParseErrorNoBacktrace, makeParseError, initial)
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Get (Get, handleError, lookAhead, many)


{- | The @ValidationError@ error type.

Error raised on failed validations against a predicate.
-}
data ValidationError = ValidationError
    deriving stock (Eq, Show)


{- | Parser that throws @t'ParseError' s e@ with specified stream and error tag.

The backtrace is a @'Nothing'@ of type @'Maybe' ('ParseError' s 'Void')@.
-}
throwParseError :: s -> e -> Get s (ParseError s e) Void
throwParseError s = throwError . makeParseErrorNoBacktrace s

{- | Parser that swallows a thrown error as a backtrace for a new error.

Combinator is useful to add contextual information, e. g. with a parser like:

@
p = do
    ...
    x <- p -- ^ Can throw here.
@

Can now be re-written as:

@
p = onParseError e q
    where
        -- If q errors with b, capture the stream and throw e with b as backtrace.
        q = do
            ...
            x <- p -- ^ Can throw here.
            ...
@
-}
{-# INLINE onParseError #-}
onParseError
    :: (Show d, Eq d, Typeable d)
    => e                            -- ^ Error tag of new thrown error.
    -> Get s (ParseError s d) a     -- ^ Parser to run.
    -> Get s (ParseError s e) a
onParseError e p = do
    s <- get
    handleError
        p
        (\ b -> throwError $ makeParseError b s e)

{- | For streamables with a notion of position, capture the position instead of the stream itself. -}
{-# INLINE withPosition #-}
withPosition
    :: HasPosition s
    => Get s (ParseError s e) a     -- ^ Parser to run.
    -> Get s (ParseError (PositionOf s) e) a
withPosition = first (first getPosition)

{- | Run the parser and return the result, validating it. -}
{-# INLINE validate #-}
validate
    :: (a -> Either d b)            -- ^ Validator.
    -> Get s (ParseError s e) a     -- ^ Parser to run.
    -> Get s (ParseError s (Either e d)) b
validate v p = do
    s <- get
    r <- first (fmap Left) p
    either
        (fmap absurd . throwParseError s . Right)
        pure
        (v r)

{- | The parser @failIff p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws a @'ParseError' s 'Void'@ if @p@ succeeds.

note(s):

    * This parser can be used to implement the longest match rule -- see 'until'.
-}
{-# INLINE failIff #-}
failIff :: Get s (ParseError s e) a -> Get s (ParseError s Void) ()
failIff p =
    first absurd (lookAhead p) >>=
        either
            (const $ pure ())
            (const $ absurd <$> throwError mempty)

{- | The parser @'until' end p@ runs @p@ zero or more times until @end@ succeeds. -}
{-# INLINE until #-}
until
    :: Get s (ParseError s e) b     -- ^ Closing parser.
    -> Get s (ParseError s e) a     -- ^ Parser to run.
    -> Get s Void [a]
until end p = many $ first initial (failIff end) *> p


{- | Guard a parser with a post-condition.

A typical use case is to ensure all input was consumed, e. g. @'guardWith' p (\ _ -> eoi)@.
-}
{-# INLINE guardWith #-}
guardWith
    :: Get s (ParseError s e) a     -- ^ Parser to run.
    -> (a -> Get s Void Bool)       -- ^ Post-condition.
    -> Get s (ParseError s (Either ValidationError e)) a
guardWith p cond = do
    x <- first (fmap Right) p
    s <- get
    b <- first absurd $ cond x
    if b
        then pure x
        else absurd <$> throwParseError s (Left ValidationError)
