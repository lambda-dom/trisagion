{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * Parsers without errors.
    value,
    observe,
) where

-- Imports.
-- Base.
import Data.Functor (($>))
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Parser (Parser, catchErrorWith)


{- | Embed a value in the t'Parser' monad.

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
