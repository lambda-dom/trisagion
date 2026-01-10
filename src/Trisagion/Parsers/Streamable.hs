{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' m a s@ constraints.
-}

module Trisagion.Parsers.Streamable (
    -- * Error types.
    ValidationError (..),

    -- * Parsers @'Streamable' m a s => 'ParserT' m s e a@.
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

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Streamable (Streamable (..), InputError (..))
import Trisagion.ParserT (ParserT, lookAhead, try)
import Trisagion.Parsers.ParseError (validate)


{- | The t'ValidationError' error tag type thrown on failed validations. -}
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)
    deriving (Applicative, Monad) via Identity


{- | Skip one element from the input stream. -}
{-# INLINE skipOne #-}
skipOne :: Streamable m a s => ParserT s Void m ()
skipOne = try one $> ()

{- | Parse one element from the input stream but without consuming input. -}
{-# INLINE peek #-}
peek :: Streamable m a s => ParserT s Void m (Maybe a)
peek = either (const Nothing) Just <$> lookAhead one

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
