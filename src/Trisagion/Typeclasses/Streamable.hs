{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Error types.
    InputError (..),
    ValidationError (..),

    -- * Typeclasses.
    Streamable (..),

    -- * Parsers @'Streamable' m a s => 'ParserT' m s e a@.
    satisfy,
    single,
    oneOf,
) where

-- Imports.
-- Base.
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (MonadState (..))

-- Non-hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
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


{- | The @Streamable@ typeclass of input streams. -}
class (Monad m, MonoFunctor a s) => Streamable m a s | s -> a where
    {-# MINIMAL one #-}

    {- | Parser getting the first element from the stream. -}
    one :: ParserT s InputError m a

    {- | Skip one element from the input stream. -}
    skipOne :: ParserT s InputError m ()
    skipOne = skip one

    {- | Parse one element from the input stream but without consuming input. -}
    peek :: ParserT s Void m (Maybe a)
    peek = either (const Nothing) Just <$> lookAhead one

    {- | Parser returning true if the input stream is exhausted. -}
    eoi :: ParserT s Void m Bool
    eoi = either (const True) (const False) <$> lookAhead one


-- Instances.
instance Monad m => Streamable m a (Maybe a) where
    {-# INLINE one #-}
    one :: ParserT (Maybe a) InputError m a
    one = do
        xs <- get
        case xs of
            Just x -> put Nothing *> pure x
            _      -> throw $ InputError 1

    {-# INLINE skipOne #-}
    skipOne :: ParserT (Maybe a) InputError m ()
    skipOne = do
        xs <- get
        case xs of
            Just _ -> put Nothing *> pure ()
            _      -> throw $ InputError 1

    {-# INLINE peek #-}
    peek :: ParserT (Maybe a) Void m (Maybe a)
    peek = get

    {-# INLINE eoi #-}
    eoi :: ParserT (Maybe a) Void m Bool
    eoi = get >>= maybe (pure True) (const $ pure False)

instance Monad m => Streamable m a [a] where
    {-# INLINE one #-}
    one :: ParserT [a] InputError m a
    one = do
        xs <- get
        case xs of
            (y : ys) -> put ys *> pure y
            _        -> throw $ InputError 1

    {-# INLINE skipOne #-}
    skipOne :: ParserT [a] InputError m ()
    skipOne = do
        xs <- get
        case xs of
            (_ : ys) -> put ys *> pure ()
            _        -> throw $ InputError 1

    {-# INLINE peek #-}
    peek :: ParserT [a] Void m (Maybe a)
    peek = get >>= pure . headMaybe
        where
            headMaybe :: [a] -> Maybe a
            headMaybe (x : _) = Just x
            headMaybe _       = Nothing

    {-# INLINE eoi #-}
    eoi :: ParserT [a] Void m Bool
    eoi = get >>= pure . null


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
