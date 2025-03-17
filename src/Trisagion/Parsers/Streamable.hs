{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Streamable (
    -- * Error types.
    InputError (..),

    -- * Parsers @'Streamable' s => 'Parser' s e a@.
    eoi,
    one,
) where

-- Imports.
-- Base.
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..), gets)

-- non-Hackage libraries.
import Data.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError)


{- | The @InputError@ error type.

Error thrown when a parser requests more input than is available.
-}
data InputError
    -- | Generic failure case.
    = InsufficientInputError
    -- | Failure case when it is possible to determine the amount requested.
    | InputError Word
    deriving stock (Eq, Show)


{- | Return @'True'@ if all input is consumed. -}
eoi :: Streamable s => Parser s Void Bool
eoi = gets isNull

{- | Parse the first @'ElementOf' s@ from the streamable. -}
one :: Streamable s => Parser s (ParseError s InputError) (ElementOf s)
one = do
    xs <- get
    case getOne xs of
        Just (y, ys) -> put ys $> y
        Nothing      -> absurd <$> throwParseError (InputError 1)
