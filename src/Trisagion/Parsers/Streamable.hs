{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' m a s@ constraints.
-}

module Trisagion.Parsers.Streamable (
    -- * Error types.
    InputError (..),
    ValidationError (..),

    -- * Parsers @'Streamable' m a s => 'ParserT' m s e a@.
    eoi,
    one,
    skipOne,
    peek,
    satisfy,
    matchOne,
    oneOf,
) where

-- Imports.
-- Base.
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.ParserT (ParserT, throw)
import Trisagion.Parsers.ParseError (validate)


{- | The t'InputError' error type. -}
newtype InputError = InputError Word
    deriving stock (Eq, Show)


{- | The t'ValidationError' error tag type thrown on failed validations. -}
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)
    deriving (Applicative, Monad) via Identity


{- | Monadic check for nullity of the input stream. -}
{-# INLINE eoi #-}
eoi :: Streamable m a s => ParserT s Void m Bool
eoi = get >>= \ xs -> lift (nullM xs)

{- | Parse one element from the input stream. -}
{-# INLINE one #-}
one :: (Streamable m a s, HasOffset m s) => ParserT s (ParseError InputError) m a
one = do
    e <- get >>= \ xs -> lift ((flip ParseError (InputError 1)) <$> offset xs)
    r <- get >>= \ xs -> lift (unconsM xs)
    case r of
        Nothing      -> throw e
        Just (x, ys) -> put ys $> x

{- | Skip one element from the input stream. -}
{-# INLINE skipOne #-}
skipOne :: Streamable m a s => ParserT s Void m ()
skipOne = do
    r <- get >>= \ xs -> lift (unconsM xs)
    case r of
        Nothing      -> pure ()
        Just (_, ys) -> put ys

{- | Extract the first element from the input stream but without consuming input. -}
{-# INLINE peek #-}
peek :: Streamable m a s => ParserT s Void m (Maybe a)
peek = do
    r <- get >>= \ xs -> lift (unconsM xs)
    case r of
        Nothing     -> pure $ Nothing
        Just (x, _) -> pure $ Just x

{- | Parse one element from the input stream satisfying a predicate. -}
{-# INLINE satisfy #-}
satisfy
    :: forall m a s . (HasOffset m s, Streamable m a s)
    => (a -> Bool)                      -- ^ Predicate on @a@.
    -> ParserT s (ParseError ((ValidationError a) :+: InputError)) m a
satisfy p = validate v one
    where
        v :: a -> ValidationError a :+: a
        v x = if p x then Left (ValidationError x) else Right x

{- | Parse one element from the input stream @'Streamable' m a s@ matching an @x :: a@. -}
{-# INLINE matchOne #-}
matchOne
    :: (HasOffset m s, Streamable m a s, Eq a)
    => a                                -- ^ Matching @x :: a@.
    -> ParserT s (ParseError ((ValidationError a) :+: InputError)) m a
matchOne x = satisfy (== x)

{- | Parse one element from the input stream @'Streamable' m a s@ matching an @x :: a@. -}
{-# INLINE oneOf #-}
oneOf
    :: (HasOffset m s, Streamable m a s, Eq a, Foldable t)
    => t a                              -- ^ Foldable of @x :: a@ against which to test inclusion.
    -> ParserT s (ParseError ((ValidationError a) :+: InputError)) m a
oneOf xs = satisfy (`elem` xs)
