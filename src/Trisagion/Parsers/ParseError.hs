{- |
Module: Trisagion.Parsers.ParseError

Parsers to handle 'Trisagion.Types.ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Handling 'Trisagion.Types.ParseError' errors.
    throwParseError,
    capture,
    onParseError,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)

-- Libraries.
import Optics ((%), review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Types.ParseError (ParseError, singleton, cons)
import Trisagion.Parser (Parser, get, throw, catch)
import Trisagion.Types.ErrorItem (errorItem)


{- | Throw @'Trisagion.Types.ParseError'@ with error @e@ and input stream the current parser state. -}
{-# INLINE throwParseError #-}
throwParseError :: e -> Parser s (ParseError s e) Void
throwParseError err = first absurd get >>= throw . review (singleton % errorItem) . (, err)

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
    x <- p -- Can throw here. If it throws, the error's input stream will be @s@.
    ...
@
-}
{-# INLINE capture #-}
capture :: Parser s (ParseError s e) a -> Parser s (ParseError s e) a
capture p = do
    xs <- first absurd get
    first (monomap (const xs)) p

{- | Parser that swallows any thrown error as a backtrace for a new error.

The input stream of the thrown error is the input stream captured /before/ @p@ runs as in the
'capture' combinator.
-}
{-# INLINE onParseError #-}
onParseError
    :: (Typeable d, Eq d, Show d)
    => e                                -- ^ Error tag of new error.
    -> Parser s (ParseError s d) a      -- ^ Parser to run.
    -> Parser s (ParseError s e) a
onParseError e p = do
    xs <- first absurd get
    catch
        p
        (fmap absurd . throw . cons xs e)
