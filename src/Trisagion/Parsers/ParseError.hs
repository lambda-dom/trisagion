{- |
Module: Trisagion.Parsers.ParseError

Parsers with t'ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Parsers with @'HasOffset' m s@ constraints.
    getOffset,

    -- * Parsers throwing t'ParseError'-errors.
    throwParseError,
    capture,
) where

-- Imports.
-- Base.
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))

-- Package.
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.ParserT (ParserT, mapError)


{- | Parser returning the current stream offset. -}
{-# INLINE getOffset #-}
getOffset :: (Monad m, HasOffset m s) => ParserT s Void m Word
getOffset = get >>= lift . offset


{- | Transform a parser throwing @e@-errors into a parser throwing (@t'ParseError' e@)-errors. -}
{-# INLINE throwParseError #-}
throwParseError
    :: (Monad m, HasOffset m s)
    => ParserT s e m a
    -> ParserT s (ParseError e) m a
throwParseError p = do
    n <- mapError absurd getOffset
    mapError (ParseError n) p

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
    :: (Monad m, HasOffset m s)
    => ParserT s (ParseError e) m a     -- ^ Parser to run.
    -> ParserT s (ParseError e) m a
capture p = do
        n  <- mapError absurd getOffset
        mapError (set n) p
    where
        set :: Word -> ParseError e -> ParseError e
        set _ Failure          = Failure
        set n (ParseError _ e) = ParseError n e
