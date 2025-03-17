{- |
Module: Trisagion.Parsers.ParseError

Parsers to handle 'ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Error types.
    -- * Handling 'ParseError'.
    throwParseError,
    onParseError,
    capture,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)
import Data.Void (Void)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Parser (Parser, catchErrorWith)
import Trisagion.Types.ParseError (ParseError, makeParseErrorNoBacktrace, makeParseError)


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

{- | Capture the stream at the entry point in case of a thrown error.

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
