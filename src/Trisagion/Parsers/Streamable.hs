{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' m a s@ constraints.
-}

module Trisagion.Parsers.Streamable (
    -- * Error types.
    ValidationError (..),

    -- * Parsers @'Streamable' m a s => 'ParserT' m s e a@.
    one,
    skipOne,
    peek,
    satisfy,
    single,
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

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.ParserT (ParserT, lookAhead, try, validate, throw)


{- | The t'InputError' error type. -}
newtype InputError = InputError Word
    deriving stock (Eq, Show)

{- | Monofunctoriality of t'InputError'. -}
instance MonoFunctor Word InputError where
    {-# INLINE monomap #-}
    monomap :: (Word -> Word) -> InputError -> InputError
    monomap f (InputError n) = InputError (f n)


{- | The t'ValidationError' error tag type thrown on failed validations. -}
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)
    deriving (Applicative, Monad) via Identity


{- | Parse one element from the input stream. -}
one :: Streamable m a s => ParserT s InputError m a
one = do
    r <- get >>= lift . unconsM
    case r of
        Nothing      -> throw (InputError 1)
        Just (x, xs) -> put xs $> x

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
    :: forall m a s . Streamable m a s
    => (a -> Bool)                      -- ^ Predicate on @a@.
    -> ParserT s (ValidationError a :+: InputError) m a
satisfy p = validate v one
    where
        v :: a -> ValidationError a :+: a
        v x = if p x then Left (ValidationError x) else Right x

{- | Parse one element from the input stream @'Streamable' m a s@ matching an @x :: a@. -}
{-# INLINE single #-}
single
    :: (Streamable m a s, Eq a)
    => a                                -- ^ Matching @x :: a@.
    -> ParserT s (ValidationError a :+: InputError) m a
single x = satisfy (== x)

{- | Parse one element from the input stream @'Streamable' m a s@ matching an @x :: a@. -}
{-# INLINE oneOf #-}
oneOf
    :: (Streamable m a s, Eq a, Foldable t)
    => t a                              -- ^ Foldable of @x :: a@ against which to test inclusion.
    -> ParserT s (ValidationError a :+: InputError) m a
oneOf xs = satisfy (`elem` xs)
