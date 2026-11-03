{-# LANGUAGE RecordWildCards #-}

{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'HasOffset' s@ constraints.
-}

module Trisagion.Parsers.HasOffset (
    -- * Parsers @'HasOffset' s => 'Parser' s e a@.
    offset,
    spanned,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (gets)

-- Package.
import Trisagion.Types.Span (Span (..))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import qualified Trisagion.Typeclasses.HasOffset as HasOffset (offset)
import Trisagion.Parser (Parser)


-- $setup
-- >>> import Trisagion.Streams.Counter
-- >>> import Trisagion.Parser
-- >>> import Trisagion.Parsers.Source


{- | Parser returning the current offset of the input stream.

=== __Examples:__

>>> parse offset $ initialize "0123"
Right (0,Counter 0 "0123")
-}
{-# INLINE offset #-}
offset :: HasOffset s => Parser s Void Int
offset = gets HasOffset.offset

{- | Run a parser and return its results along with the consumed span.

=== __Examples:__

>>> parse (spanned one) $ initialize "0123"
Right (('0',Span {lower = 0, upper = 1}),Counter 1 "123")
-}
{-# INLINE spanned #-}
spanned :: HasOffset s => Parser s e a -> Parser s e (a, Span)
spanned p = do
    lower <- first absurd offset
    x     <- p
    upper <- first absurd offset
    pure (x, Span {..})
