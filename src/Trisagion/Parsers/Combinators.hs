{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * Parsers without errors.
    value,
    observe,
    lookAhead,
    maybe,
) where

-- Imports.
-- Prelude.
import Prelude hiding (either, maybe)
import Prelude as Base (either)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Parser (Parser, catchErrorWith, eval)


{- | Embed a value in the 'Parser' monad.

The difference with 'pure' from 'Applicative' is the more precise signature.
-}
value :: a -> Parser s Void a
value = pure

{- | Run the parser and return the result as a 'Right'; on error, backtrack and return it as a 'Left'. -}
observe :: Parser s e a -> Parser s Void (Either e a)
observe p = do
    s <- get
    catchErrorWith
        (Right <$> p)
        (\ e -> put s $> Left e)

{- | Run the parser and return the result, but do not consume any input. -}
lookAhead :: Parser s e a -> Parser s Void (Either e a)
lookAhead p = eval p <$> first absurd get

{- | Run the parser and return the result as a @'Just'@. If it errors, backtrack and return @'Nothing'@.

The difference with @'Control.Applicative.optional'@ is the more precise type signature.
-}
maybe :: Parser s e a -> Parser s Void (Maybe a)
maybe p = Base.either (const Nothing) Just <$> observe p

