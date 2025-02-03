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
import Control.Monad.State (MonadState (..), gets)
import Data.MonoTraversable (MonoFoldable (..), Element)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Get (Get, lookAhead)
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

{- | Parse the first @'Element' s@ from the streamable. -}
{-# INLINE one #-}
one
    :: Streamable s
    => Get s (ParseError s InputError) (Element s)
one = do
    xs <- get
    case getOne xs of
        Just (y, ys) -> put ys $> y
        Nothing      -> absurd <$> throwParseError xs (InputError 1)

{- | Extract the first @'Element' s@ from the streamable but without consuming input. -}
{-# INLINE peek #-}
peek
    :: Streamable s
    => Get s Void (Either (ParseError s InputError) (Element s))
peek = lookAhead one

{- | Parse one @'Element' s@ satisfying a predicate. -}
{-# INLINE satisfy #-}
satisfy
    :: Streamable s
    => (Element s -> Bool)          -- ^ @'Element' s@ predicate.
    -> Get s (ParseError s (Either InputError ValidationError)) (Element s)
satisfy p = validate v one
    where
        v x = if p x then Right x else Left ValidationError

{- | Parse one matching @'Element' s@. -}
{-# INLINE matchElem #-}
matchElem
    :: (Streamable s, Eq (Element s))
    => Element s                    -- ^ Matching @'Element' s@.
    -> Get s (ParseError s (Either InputError (MatchError (Element s)))) (Element s)
matchElem x = first (fmap (fmap (const $ MatchError x))) $ satisfy (== x)

{- | Parse one @'Element' s@ that is an element of the foldable. -}
{-# INLINE oneOf #-}
oneOf
    :: (Streamable s, Eq (Element s), Foldable t)
    => t (Element s)                -- ^ Foldable of @'Element' s@ against which to test inclusion.
    -> Get s (ParseError s (Either InputError (MatchError (t (Element s))))) (Element s)
oneOf xs = first (fmap (fmap (const $ MatchError xs))) $ satisfy (`elem` xs)
