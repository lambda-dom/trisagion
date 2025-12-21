{- |
Module: Trisagion.Parsers.ParseError

Parsers for handling t'ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * t'ParseError' parsers.
    throwParseError,
    capture,
    validate,
) where

-- Imports.
import Data.Bifunctor (Bifunctor (..))

-- Libraries.
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.ParserT (ParserT, lift, throw)


{- | Throw @t'ParseError'@ with error tag @e@ and offset the current stream offset. -}
{-# INLINE throwParseError #-}
throwParseError :: (Monad m, HasOffset m s) => e -> ParserT m s (ParseError e) a
throwParseError e = do
    err <- get >>= \ xs -> lift ((flip ParseError e) <$> offset xs)
    throw err

{- | Capture the offset of the input stream at the entry point in case of an error.

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
    -- Capture the offset @n@ of the input stream here.
    ...
    x <- p -- Can throw here. If it throws, the error's offset will be @n@.
    ...
@
-}
{-# INLINE capture #-}
capture :: (Monad m, HasOffset m s) => ParserT m s (ParseError e) a -> ParserT m s (ParseError e) a
capture p = do
        n  <- get >>= \ xs -> lift (offset xs)
        first (set n) p
    where
        set :: Word -> ParseError e -> ParseError e
        set _ Failure          = Failure
        set n (ParseError _ e) = ParseError n e

{- | Run the parser and return the result, validating it. -}
{-# INLINE validate #-}
validate
    :: (Monad m, HasOffset m s)
    => (a -> d :+: b)                   -- ^ Validator.
    -> ParserT m s (ParseError e) a     -- ^ Parser to run.
    -> ParserT m s (ParseError (d :+: e)) b
validate v p = do
    x <- first (fmap Right) p
    case v x of
        Left d  -> throwParseError (Left d)
        Right y -> pure y
