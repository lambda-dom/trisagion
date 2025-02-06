{- |
Module: Trisagion.Getters.Streamable

Parsers with @'Streamable' s@ constraints on the state @s@.
-}

module Trisagion.Getters.Streamable (
    -- * Error types.
    InputError (..),
    MatchError (..),

    -- * @'Streamable' s => 'Get' s e a@ parsers.
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
import Control.Monad.State (gets)
import Data.MonoTraversable (MonoFoldable (..))

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.Streamable (Streamable (..), ElementOf)
import Trisagion.Get (Get, lookAhead, get, put)
import Trisagion.Getters.ParseError (ValidationError (..), throwParseError, validate)


{- | The @InputError@ error type.

Error raised when a parser requests more input than is available.
-}
data InputError
    = InsufficientInputError    -- ^ Generic failure case.
    | InputError Word           -- ^ Failure case when it is possible to determine the amount requested.
    deriving stock (Eq, Show)

{- | The @MatchError@ error type.

Error raised on failed validations against predicates of the form @p x y@ such as equality, element
tests, etc.
-}
newtype MatchError a = MatchError a
    deriving stock (Eq, Show)


{- | Return @'True'@ if all input is consumed. -}
{-# INLINE eoi #-}
eoi :: Streamable s => Get s Void Bool
eoi = gets onull

{- | Parse the first @'ElementOf' s@ from the streamable. -}
{-# INLINE one #-}
one
    :: Streamable s
    => Get s (ParseError s InputError) (ElementOf s)
one = do
    xs <- get
    case getOne xs of
        Just (y, ys) -> put ys $> y
        Nothing      -> absurd <$> throwParseError xs (InputError 1)

{- | Extract the first @'ElementOf' s@ from the streamable but without consuming input. -}
{-# INLINE peek #-}
peek
    :: Streamable s
    => Get s Void (Either (ParseError s InputError) (ElementOf s))
peek = lookAhead one

{- | Parse one @'ElementOf' s@ satisfying a predicate. -}
{-# INLINE satisfy #-}
satisfy
    :: Streamable s
    => (ElementOf s -> Bool)          -- ^ @'ElementOf' s@ predicate.
    -> Get s (ParseError s (Either InputError ValidationError)) (ElementOf s)
satisfy p = validate v one
    where
        v x = if p x then Right x else Left ValidationError

{- | Parse one matching @'ElementOf' s@. -}
{-# INLINE matchElem #-}
matchElem
    :: (Streamable s, Eq (ElementOf s))
    => ElementOf s                    -- ^ Matching @'ElementOf' s@.
    -> Get s (ParseError s (Either InputError (MatchError (ElementOf s)))) (ElementOf s)
matchElem x = first (fmap (fmap (const $ MatchError x))) $ satisfy (== x)

{- | Parse one @'ElementOf' s@ that is an element of the foldable. -}
{-# INLINE oneOf #-}
oneOf
    :: (Streamable s, Eq (ElementOf s), Foldable t)
    => t (ElementOf s)                -- ^ Foldable of @'ElementOf' s@ against which to test inclusion.
    -> Get s (ParseError s (Either InputError (MatchError (t (ElementOf s))))) (ElementOf s)
oneOf xs = first (fmap (fmap (const $ MatchError xs))) $ satisfy (`elem` xs)
