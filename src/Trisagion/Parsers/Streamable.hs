{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' m a s@ constraints.
-}

module Trisagion.Parsers.Streamable (
    -- * Error types.
    InputError (..),
    ValidationError (..),

    -- * Parsers @'Streamable' m a s => 'ParserT' m s e a@.
    one,
    eoi,
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
import Data.Kind (Type)
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.ParserT (ParserT, throw, lookAhead, validate)
import Trisagion.Parsers.Combinators (skip)


{- | The @InputError@ error type. -}
type InputError :: Type
newtype InputError = InputError Word
    deriving stock (Eq, Show)

{- | Monofunctoriality of t'InputError'. -}
instance MonoFunctor Word InputError where
    {-# INLINE monomap #-}
    monomap :: (Word -> Word) -> InputError -> InputError
    monomap f (InputError n) = InputError (f n)


{- | The @ValidationError@ error tag type thrown on failed validations. -}
type ValidationError :: Type -> Type
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)
    deriving (Applicative, Monad) via Identity


{- | Parse one element from the input stream. -}
{-# INLINE one #-}
one :: Streamable m a s => ParserT s InputError m a
one = do
    r <- get >>= lift . unconsM
    case r of
        Just (x, xs) -> put xs $> x
        _            -> throw $ InputError 1

{- | Parser returning 'True' if there are no more elements in the stream. -}
{-# INLINE eoi #-}
eoi :: Streamable m a s => ParserT s Void m Bool
eoi = get >>= lift . nullM

{- | Skip one element from the input stream. -}
{-# INLINE skipOne #-}
skipOne :: Streamable m a s => ParserT s InputError m ()
skipOne = skip one

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

{- | Parse one element from the input stream matching an @x :: a@. -}
{-# INLINE single #-}
single
    :: (Streamable m a s, Eq a)
    => a                                -- ^ Matching @x :: a@.
    -> ParserT s (ValidationError a :+: InputError) m a
single x = satisfy (== x)

{- | Parse one element from the input stream that is an element of a foldable. -}
{-# INLINE oneOf #-}
oneOf
    :: (Streamable m a s, Eq a, Foldable t)
    => t a                              -- ^ Foldable of @a@'s against which to test inclusion.
    -> ParserT s (ValidationError a :+: InputError) m a
oneOf xs = satisfy (`elem` xs)
