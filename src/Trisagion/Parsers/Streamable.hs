{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Streamable (
    -- * Error types.
    InputError (..),

    -- * Parsers @'Streamable' s => 'Parser' s e a@.
    eoi,
) where

-- Imports.
-- Base.
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (gets)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Parser (Parser)


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
