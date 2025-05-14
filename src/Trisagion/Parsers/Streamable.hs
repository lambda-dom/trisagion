{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Streamable (
    -- * Parsers @'Streamable' s => 'Parser' s e a@.
    eoi,
    ensureEOI,
    peek,
    satisfy,
    matchElem,
    oneOf,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (null)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Parser (Parser, (:+:), get, one)
import Trisagion.Parsers.Combinators (lookAhead)
import Trisagion.Parsers.ParseError (ValidationError, validate)


{- | Return @'True'@ if all input is consumed. -}
{-# INLINE eoi #-}
eoi :: Streamable s => Parser s Void Bool
eoi = null <$> get

{- | Run parser @p@ and if not all input is consumed, error out. -}
{-# INLINE ensureEOI #-}
ensureEOI :: Streamable s => d -> Parser s e a -> Parser s (d :+: e) a
ensureEOI err p = do
    x <- first Right p
    b <- first absurd eoi
    if b
        then pure x
        else throwError $ Left err

{- | Extract the first @'ElementOf' s@ from the streamable but without consuming input. -}
{-# INLINE peek #-}
peek :: Streamable s => Parser s Void (Maybe (ElementOf s))
peek = either (const Nothing) Just <$> lookAhead one

{- | Parse one @'ElementOf' s@ satisfying a predicate. -}
{-# INLINE satisfy #-}
satisfy
    :: Streamable s
    => (ElementOf s -> Bool)            -- ^ @'ElementOf' s@ predicate.
    -> Parser s (ParseError s (ValidationError (ElementOf s))) (ElementOf s)
satisfy p = first (fmap (either id absurd)) $ validate v one
    where
        v x = if p x then Right x else Left $ pure x

{- | Parse one element matching a @'ElementOf' s@. -}
{-# INLINE matchElem #-}
matchElem
    :: (Streamable s, Eq (ElementOf s))
    => ElementOf s                      -- ^ Matching @'ElementOf' s@.
    -> Parser s (ParseError s (ValidationError (ElementOf s))) (ElementOf s)
matchElem x = satisfy (== x)

{- | Parse one @'ElementOf' s@ that is an element of a foldable. -}
{-# INLINE oneOf #-}
oneOf
    :: (Streamable s, Eq (ElementOf s), Foldable t)
    => t (ElementOf s)                  -- ^ Foldable of @'ElementOf' s@ to test inclusion.
    -> Parser s (ParseError s (ValidationError (ElementOf s))) (ElementOf s)
oneOf xs = satisfy (`elem` xs)
