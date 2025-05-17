{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Streamable (
    -- * Parsers @'Streamable' s => 'Parser' s e a@.
    eoi,
    ensureEOI,
    peek,

    -- * Parsers @'HasOffset' s => 'Parser' s e a@.
    matchElem,
    oneOf,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (null)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void, absurd)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Types.ParseError (ParseError, ValidationError)
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Parser
import Trisagion.Parsers.Combinators (lookAhead)


{- | Return @'True'@ if all input is consumed.

=== __Examples:__

>>> parse eoi "0123"
Right (False,"0123")

>>> parse eoi ""
Right (True,"")
-}
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
        else throw $ Left err

{- | Extract the first @'ElementOf' s@ from the streamable but without consuming input.

=== __Examples:__

>>> parse peek "0123"
Right (Just '0',"0123")

>>> parse peek ""
Right (Nothing,"")
-}
{-# INLINE peek #-}
peek :: Streamable s => Parser s Void (Maybe (ElementOf s))
peek = either (const Nothing) Just <$> lookAhead one


{- | Parse one element matching a @'ElementOf' s@. -}
{-# INLINE matchElem #-}
matchElem
    :: (HasOffset s, Eq (ElementOf s))
    => ElementOf s                      -- ^ Matching @'ElementOf' s@.
    -> Parser s (ParseError (ValidationError (ElementOf s))) (ElementOf s)
matchElem x = satisfy (== x)

{- | Parse one @'ElementOf' s@ that is an element of a foldable. -}
{-# INLINE oneOf #-}
oneOf
    :: (HasOffset s, Eq (ElementOf s), Foldable t)
    => t (ElementOf s)                  -- ^ Foldable of @'ElementOf' s@ to test inclusion.
    -> Parser s (ParseError (ValidationError (ElementOf s))) (ElementOf s)
oneOf xs = satisfy (`elem` xs)
