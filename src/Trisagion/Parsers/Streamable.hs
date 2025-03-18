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
    peek,
    satisfy,
    matchElem,
    oneOf,
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
import Trisagion.Parsers.Combinators (lookAhead)
import Trisagion.Parsers.ParseError (ValidationError (..), throwParseError, validate)


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
    case splitOne xs of
        Just (y, ys) -> put ys $> y
        Nothing      -> absurd <$> throwParseError (InputError 1)

{- | Extract the first @'ElementOf' s@ from the streamable but without consuming input. -}
peek :: Streamable s => Parser s Void (Either (ParseError s InputError) (ElementOf s))
peek = lookAhead one

{- | Parse one @'ElementOf' s@ satisfying a predicate. -}
satisfy
    :: Streamable s
    => (ElementOf s -> Bool)            -- ^ @'ElementOf' s@ predicate.
    -> Parser s (ParseError s (Either InputError (ValidationError (ElementOf s)))) (ElementOf s)
satisfy p = validate v one
    where
        v x = if p x then Right x else Left $ ValidationError x

{- | Parse one element matching a @'ElementOf' s@. -}
matchElem
    :: (Streamable s, Eq (ElementOf s))
    => ElementOf s                      -- ^ Matching @'ElementOf' s@.
    -> Parser s (ParseError s (Either InputError (ValidationError (ElementOf s)))) (ElementOf s)
matchElem x = satisfy (== x)

{- | Parse one @'ElementOf' s@ that is an element of a foldable. -}
oneOf
    :: (Streamable s, Eq (ElementOf s), Foldable t)
    => t (ElementOf s)                -- ^ Foldable of @'ElementOf' s@ against which to test inclusion.
    -> Parser s (ParseError s (Either InputError (ValidationError (ElementOf s)))) (ElementOf s)
oneOf xs = satisfy (`elem` xs)
