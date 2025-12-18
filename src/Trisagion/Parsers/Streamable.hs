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
) where

-- Imports.
-- Base.
import Data.Void (Void)

-- Package.
import Trisagion.Types.Result (Result (..), (:+:))
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.ParserT (ParserT, embed)
import Trisagion.Parsers.ParseError (validate)


{- | The t'InputError' error type. -}
newtype InputError = InputError Word
    deriving stock (Eq, Show)


{- | The t'ValidationError' error tag type thrown on failed validations. -}
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)


{- | Monadic check for nullity of the input stream. -}
{-# INLINE eoi #-}
eoi :: Streamable m a s => ParserT m s Void Bool
eoi = embed $ \ xs -> do
    r <- nullM xs
    pure (Success r xs)

{- | Parse one element from the input stream. -}
{-# INLINE one #-}
one :: (Streamable m a s, HasOffset m s) => ParserT m s (ParseError InputError) a
one = embed $ \ xs -> do
    n <- offset xs
    r <- unconsM xs
    case r of
        Nothing      -> pure $ Error (ParseError n (InputError 1))
        Just (x, ys) -> pure $ Success x ys

{- | Skip one element from the input stream. -}
{-# INLINE skipOne #-}
skipOne :: Streamable m a s => ParserT m s Void ()
skipOne = embed $ \ xs -> do
    r <- unconsM xs
    case r of
        Nothing      -> pure $ Success () xs
        Just (_, ys) -> pure $ Success () ys

{- | Extract the first element from the input stream but without consuming input. -}
{-# INLINE peek #-}
peek :: Streamable m a s => ParserT m s Void (Maybe a)
peek = embed $ \ xs -> do
    r <- unconsM xs
    case r of
        Nothing     -> pure $ Success Nothing xs
        Just (x, _) -> pure $ Success (Just x) xs

{- | Parse one element from the input stream satisfying a predicate. -}
{-# INLINE satisfy #-}
satisfy
    :: forall m a s . (HasOffset m s, Streamable m a s)
    => (a -> Bool)            -- ^ @a@-predicate.
    -> ParserT m s (ParseError ((ValidationError a) :+: InputError)) a
satisfy p = validate v one
    where
        v :: a -> ValidationError a :+: a
        v x = if p x then Left (ValidationError x) else Right x
