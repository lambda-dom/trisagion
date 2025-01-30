{- |
Module: Trisagion.Getters.ParseError

Parser combinators relying on special error handling with the @'ParseError'@ type.
-}

module Trisagion.Getters.ParseError (
    -- * Type aliases.
    GetPE,

    -- * Error types.
    ValidationError (..),

    -- * Handling t'ParseError'.
    throwParseError,
    throwParseErrorWithStream,
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
type GetPE s e a = Get s (ParseError (PositionOf s) e) a


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
throwParseError :: HasPosition s => e -> GetPE s e Void
throwParseError e = embed $ Error . flip makeParseErrorNoBacktrace e

{- | Parser that throws @t'ParseError' ('PositionOf' s) e@ with specified stream and error tag.

The backtrace is a @'Nothing'@ of type @'Maybe' ('ParseError' ('PositionOf' s) 'Void')@.
-}
throwParseErrorWithStream :: HasPosition s => s -> e -> GetPE s e Void
throwParseErrorWithStream xs = throwError . makeParseErrorNoBacktrace xs

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
    -> GetPE s d a                  -- ^ Parser to run.
    -> GetPE s e a
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
    -> GetPE s e a                  -- ^ Parser to run.
    -> GetPE s (Either e d) b
validate v p = do
    s <- get
    r <- first (fmap Left) p
    either
        (fmap absurd . throwParseErrorWithStream s . Right)
        pure
        (v r)

{- | The parser @failIff p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws a @'ParseError' ('PositionOf' s) 'Void'@ if @p@ succeeds.

note(s):

    * This parser can be used to implement the longest match rule -- see 'until'.
-}
{-# INLINE failIff #-}
failIff :: GetPE s e a -> GetPE s Void ()
failIff p =
    first absurd (lookAhead p) >>=
        either
            (const $ pure ())
            (const $ absurd <$> throwError mempty)

{- | The parser @'until' end p@ runs @p@ zero or more times until @end@ succeeds. -}
{-# INLINE until #-}
until
    :: GetPE s e b                  -- ^ Closing parser.
    -> GetPE s e a                  -- ^ Parser to run.
    -> Get s Void [a]
until end p = many $ first initial (failIff end) *> p


{- | Guard a parser with a post-condition.

A typical use case is to ensure all input was consumed, e. g. @'guardWith' p (\ _ s -> onull s)@.
-}
{-# INLINE guardWith #-}
guardWith
    :: HasPosition s
    => GetPE s e a                  -- ^ Parser to run.
    -> (s -> a -> Bool)             -- ^ Post-condition.
    -> GetPE s (Either ValidationError e) a
guardWith p cond = do
    x <- first (fmap Right) p
    s <- get
    if cond s x
        then pure x
        else absurd <$> throwParseError (Left ValidationError)
