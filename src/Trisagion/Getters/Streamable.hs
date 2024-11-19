{- |
Module: Trisagion.Getters.Streamable

Parsers with @'Streamable' s@ constraints on the state @s@.
-}

module Trisagion.Getters.Streamable (
    -- * Error types.
    InputError (..),
    ValidationError (..),
    MatchError (..),

    -- * @'Streamable' s => t'Get' s e a@ parsers.
    eoi,
    one,
    peek,
    satisfy,
    matchElem,
    oneOf,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..), gets)
import Data.MonoTraversable (Element, MonoFoldable (..))

-- Package.
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Get (Get, throwParseError)
import Trisagion.Getters.Combinators (lookAhead, validate)


{- | The @InputError@ error type.

Error raised when a parser requests more input than is available.
-}
data InputError
    = InsufficientInputError    -- ^ Generic failure case.
    | InputError Word           -- ^ Failure case when it is possible to determine the amount requested.
    deriving stock (Eq, Show)

{- | The @ValidationError@ error type.

Error raised on failed validations against a predicate.
-}
data ValidationError = ValidationError
    deriving stock (Eq, Show)

{- | The @MatchError@ error type.

Error raised on failed validations against predicates of the form @p x y@ such as equality, element
tests, etc.
-}
newtype MatchError a = MatchError a
    deriving stock (Eq, Show)


{- | Return @'True'@ if all input is consumed. -}
eoi :: Streamable s => Get s Void Bool
eoi = gets onull

{- | Get the first @'Element' s@ from the streamable. -}
one
    :: Streamable s
    => Get s (ParseError s InputError) (Element s)
one = do
    xs <- get
    case getOne xs of
        Just (y, ys) -> put ys $> y
        Nothing      -> absurd <$> throwParseError (InputError 1)

{- | Extract the first @'Element' s@ from the streamable but without consuming input. -}
peek
    :: Streamable s
    => Get s Void (Either (ParseError s InputError) (Element s))
peek = lookAhead one

{- | Get one @'Element' s@ from the streamable satisfying a predicate. -}
satisfy
    :: Streamable s
    => (Element s -> Bool)    -- ^ @'Element' s@ predicate.
    -> Get s (ParseError s (Either InputError ValidationError)) (Element s)
satisfy p = validate v one
    where
        v x = if p x then Right x else Left ValidationError

{- | Get one matching @'Element' s@ from the streamable @s@. -}
matchElem
    :: (Streamable s, Eq (Element s))
    => Element s               -- ^ Matching @'Element' s@.
    -> Get s (ParseError s (Either InputError (MatchError (Element s)))) (Element s)
matchElem x = first (fmap (fmap (const $ MatchError x))) $ satisfy (== x)

{- | Get one @'Element' s@ from the stream if it is an element of the foldable. -}
oneOf
    :: (Streamable s, Eq (Element s), Foldable t)
    => t (Element s)           -- ^ Foldable of @'Element' s@ against which to test inclusion.
    -> Get s (ParseError s (Either InputError (MatchError (t (Element s))))) (Element s)
oneOf xs = first (fmap (fmap (const $ MatchError xs))) $ satisfy (`elem` xs)
