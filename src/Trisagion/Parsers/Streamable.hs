{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Streamable (
    -- * Parsers @'Streamable' s => 'Parser' s e a@.
    eoi,
    peek,
    satisfy,
    matchElem,
    oneOf,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (null)

-- Base.
import Data.Void (Void)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser, ParserPE, InputError, (:+:), get, one)
import Trisagion.Parsers.Combinators (lookAhead)
import Trisagion.Parsers.ParseError (ValidationError (..), validate)


{- | Return @'True'@ if all input is consumed. -}
eoi :: Streamable s => Parser s Void Bool
eoi = null <$> get

{- | Extract the first @'ElementOf' s@ from the streamable but without consuming input. -}
peek
    :: (HasPosition s, Streamable s)
    => Parser s Void (ParseError (PositionOf s) InputError :+: ElementOf s)
peek = lookAhead one

{- | Parse one @'ElementOf' s@ satisfying a predicate. -}
satisfy
    :: (HasPosition s, Streamable s)
    => (ElementOf s -> Bool)            -- ^ @'ElementOf' s@ predicate.
    -> ParserPE s (InputError :+: ValidationError (ElementOf s)) (ElementOf s)
satisfy p = validate v one
    where
        v x = if p x then Right x else Left $ ValidationError x

{- | Parse one element matching a @'ElementOf' s@. -}
matchElem
    :: (HasPosition s, Streamable s, Eq (ElementOf s))
    => ElementOf s                      -- ^ Matching @'ElementOf' s@.
    -> ParserPE s (InputError :+: ValidationError (ElementOf s)) (ElementOf s)
matchElem x = satisfy (== x)

{- | Parse one @'ElementOf' s@ that is an element of a foldable. -}
oneOf
    :: (HasPosition s, Streamable s, Eq (ElementOf s), Foldable t)
    => t (ElementOf s)                -- ^ Foldable of @'ElementOf' s@ against which to test inclusion.
    -> ParserPE s (InputError :+: ValidationError (ElementOf s)) (ElementOf s)
oneOf xs = satisfy (`elem` xs)
