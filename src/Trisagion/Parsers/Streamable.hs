{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' a s@ constraints.
-}

module Trisagion.Parsers.Streamable (
    -- * Error types.
    InputError (..),
    ValidationError (..),

    -- * Parsers @'Streamable' a s => 'Parser' s e a@.
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
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Except (MonadError (..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import qualified Trisagion.Typeclasses.Streamable as Streamable (null)
import Trisagion.Parser (Parser, lookAhead, validate)
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
one :: Streamable a s => Parser s InputError a
one = do
    r <- gets uncons
    case r of
        Just (x, xs) -> put xs $> x
        _            -> throwError $ InputError 1

{- | Parser returning 'True' if there are no more elements in the stream. -}
{-# INLINE eoi #-}
eoi :: Streamable a s => Parser s Void Bool
eoi = gets Streamable.null

{- | Skip one element from the input stream. -}
{-# INLINE skipOne #-}
skipOne :: Streamable a s => Parser s InputError ()
skipOne = skip one

{- | Parse one element from the input stream but without consuming input. -}
{-# INLINE peek #-}
peek :: Streamable a s => Parser s Void (Maybe a)
peek = either (const Nothing) Just <$> lookAhead one

{- | Parse one element from the input stream satisfying a predicate. -}
{-# INLINE satisfy #-}
satisfy
    :: forall a s . Streamable a s
    => (a -> Bool)                      -- ^ Predicate on @a@.
    -> Parser s (ValidationError a :+: InputError) a
satisfy p = validate v one
    where
        v :: a -> ValidationError a :+: a
        v x = if p x then Left (ValidationError x) else Right x

{- | Parse one element from the input stream matching an @x :: a@. -}
{-# INLINE single #-}
single
    :: (Streamable a s, Eq a)
    => a                                -- ^ Matching @x :: a@.
    -> Parser s (ValidationError a :+: InputError) a
single x = satisfy (== x)

{- | Parse one element from the input stream that is an element of a foldable. -}
{-# INLINE oneOf #-}
oneOf
    :: (Streamable a s, Eq a, Foldable t)
    => t a                              -- ^ Foldable of @a@'s against which to test inclusion.
    -> Parser s (ValidationError a :+: InputError) a
oneOf xs = satisfy (`elem` xs)
