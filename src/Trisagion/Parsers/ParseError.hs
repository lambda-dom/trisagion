{- |
Module: Trisagion.Parsers.ParseError

Parsers for handling t'ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Error parsers.
    throwParseError,
    capture,
    validate,
) where

-- Imports.
import Data.Bifunctor (Bifunctor (..))

-- Package.
import Trisagion.Types.Result (Result (..), (:+:))
import Trisagion.Types.ParseError (ParseError (..), makeParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.ParserT (ParserT, embed, run)


{- | Throw @t'ParseError'@ with error tag @e@ and offset the current stream offset. -}
{-# INLINE throwParseError #-}
throwParseError :: (Monad m, HasOffset m s) => e -> ParserT m s (ParseError e) a
throwParseError e = embed $ \ xs -> do
    err <- makeParseError xs e
    pure $ Error err

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
capture p = embed $ \ xs -> do
        n <- offset xs
        fmap (first (set n)) $ run p xs
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
