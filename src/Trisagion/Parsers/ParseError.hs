{- |
Module: Trisagion.Parsers.ParseError

Parsers to handle 'ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Error types.
    ValidationError (..),

    -- * Handling 'ParseError'.
    throwParseError,
    onParseError,
    capture,

    -- * Parsers without errors.
    failIff,
    until,

    -- * Validators.
    validate,
    guardWith,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (until)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Types.ParseError (ParseError, makeParseErrorNoBacktrace, makeParseError, initial)
import Trisagion.Parser (Parser, catchErrorWith)
import Trisagion.Parsers.Combinators (lookAhead, many)


{- | The @ValidationError@ error type.

Error thrown on failed validations against a predicate.
-}
newtype ValidationError a = ValidationError a
    deriving stock (Eq, Show, Functor)


{- | Parser that throws @'ParseError' s e@ with error tag @e@.

The state component is the current parser state and the backtrace is a @'Nothing'@ of type
@'Maybe' ('ParseError' s 'Void')@.
-}
throwParseError :: e -> Parser s (ParseError s e) Void
throwParseError e = get >>= throwError . flip makeParseErrorNoBacktrace e

{- | Parser that swallows any thrown error as a backtrace for a new error. -}
onParseError
    :: (Typeable d, Eq d, Show d)
    => e                                -- ^ Error tag of new error.
    -> Parser s (ParseError s d) a      -- ^ Parser to run.
    -> Parser s (ParseError s e) a
onParseError e p =
    catchErrorWith
        p
        (\ b -> do
            s <- get
            throwError $ makeParseError b s e)

{- | Capture the input stream at the entry point in case of a thrown error.

A parser,

@
parser = do
    ...
    x <- p -- Can throw here.
    ...
@

can now be written as:

@
parser = capture $ do
    -- Capture the input stream @s@ here.
    ...
    x <- p -- Can throw here. If it throws, the error's stream input will be @s@.
    ...
@
-}
capture :: Parser s (ParseError s e) a -> Parser s (ParseError s e) a
capture p = do
    s <- get
    first (first (const s)) p


{- | The parser @failIff p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws a @'ParseError' s 'Void'@ if @p@ succeeds.

note(s):

    * This parser can be used to implement the longest match rule -- see 'until'.
-}
failIff :: Parser s (ParseError s e) a -> Parser s (ParseError s Void) ()
failIff p =
    first absurd (lookAhead p) >>=
        either
            (const $ pure ())
            (const $ absurd <$> throwError mempty)

{- | The parser @'until' end p@ runs @p@ zero or more times until @end@ succeeds. -}
until
    :: Parser s (ParseError s e) b      -- ^ Closing parser.
    -> Parser s (ParseError s e) a      -- ^ Parser to run.
    -> Parser s Void [a]
until end p = many $ first initial (failIff end) *> p


{- | Run the parser and return the result, validating it. -}
validate
    :: (a -> Either d b)                -- ^ Validator.
    -> Parser s (ParseError s e) a      -- ^ Parser to run.
    -> Parser s (ParseError s (Either e d)) b
validate v p = capture $ do
    r <- first (fmap Left) p
    either
        (fmap absurd . throwParseError . Right)
        pure
        (v r)

{- | Guard a parser with a monadic post-condition.

A generalization of 'validate', where the post-condition can depend on the parser state. The
post-condition parser is not allowed to throw an error, because first, ideally it should not
consume input and second, the error part of the type signature is already complicated.

An example is to ensure all input was consumed, e. g. @'guardWith' (const eoi) p@.
-}
guardWith
    :: (a -> Parser s Void Bool)        -- ^ Post-condition.
    -> Parser s (ParseError s e) a      -- ^ Parser to run.
    -> Parser s (ParseError s (Either (ValidationError a) e)) a
guardWith cond p = capture $ do
    x <- first (fmap Right) p
    b <- first absurd $ cond x
    if b
        then pure x
        else absurd <$> throwParseError (Left (ValidationError x))
