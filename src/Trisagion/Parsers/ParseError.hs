{- |
Module: Trisagion.Parsers.ParseError

Parsers with t'ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Parsers throwing t'ParseError'-errors.
    throwParseError,
    capture,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (absurd)

-- Package.
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.HasOffset (offset)


{- | Transform a parser throwing @e@-errors into a parser throwing (@t'ParseError' e@)-errors. -}
{-# INLINE throwParseError #-}
throwParseError
    :: HasOffset s
    => Parser s e a
    -> Parser s (ParseError e) a
throwParseError p = do
    n <- first absurd offset
    first (ParseError n) p

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
    -- Capture the offset @n@ here.
    ...
    x <- p -- Can throw here. If it throws, the error's offset will be @n@.
    ...
@
-}
{-# INLINE capture #-}
capture
    :: HasOffset s
    => Parser s (ParseError e) a        -- ^ Parser to run.
    -> Parser s (ParseError e) a
capture p = do
        n  <- first absurd offset
        first (set n) p
    where
        set :: Word -> ParseError e -> ParseError e
        set _ Failure          = Failure
        set n (ParseError _ e) = ParseError n e
