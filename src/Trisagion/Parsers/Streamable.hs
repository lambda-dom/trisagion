{- |
Module: Trisagion.Parsers.ParseError

Parsers wth @'Streamable' s@ constraints.
-}

module Trisagion.Parsers.Streamable (
    -- * Type aliases.
    InputError,

    -- * Parsers @'Streamable' s => 'Parser' s e a@.
    eoi,
    ensureEOI,
    one,
    skipOne,
    peek,
    satisfy,
    matchOne,
    oneOf,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..), gets, modify)
import Optics.Core ((%), review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import qualified Trisagion.Typeclasses.Streamable as Streamable (null)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Types.ErrorItem (endOfInput)
import Trisagion.Types.ParseError (ParseError, ValidationError, singleton)
import Trisagion.Parser (Parser, (:+:), throw)
import Trisagion.Parsers.ParseError (throwParseError)


{- | Type alias to make signatures of parsers that only fail on insufficient input clearer. -}
type InputError = ParseError Void


{- | Return @'True'@ if all input is consumed.

=== __Examples:__

>>> parse eoi "0123"
Right (False,"0123")

>>> parse eoi ""
Right (True,"")
-}
{-# INLINE eoi #-}
eoi :: Streamable s => Parser s Void Bool
eoi = gets Streamable.null

{- | Run parser @p@ and if not all input is consumed, error out.

=== __Examples:__

>>> parse (ensureEOI () (one *> one)) "01"
Right ('1',"")

>>> parse (ensureEOI () one) "01"
Left (Left ())
-}
{-# INLINE ensureEOI #-}
ensureEOI :: Streamable s => d -> Parser s e a -> Parser s (d :+: e) a
ensureEOI err p = do
    x <- first Right p
    b <- first absurd eoi
    if b
        then pure x
        else throw $ Left err

{- | Parse one @'ElementOf' s@ from the input stream.

=== __Examples:__

>>> parse one "0123"
Right ('0',"123")

>>> parse one ""
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE one #-}
one :: Streamable s => Parser s InputError (ElementOf s)
one = do
    xs <- get
    case uncons xs of
        Nothing -> throw $ review (singleton % endOfInput) 1
        Just (x, ys) -> put ys $> x

{- | Skip one @'ElementOf' s@ from the input stream.

=== __Examples:__

>>> parse skipOne "0123"
Right ((),"123")

>>> parse skipOne ""
Right ((),"")
 -}
{-# INLINE skipOne #-}
skipOne :: Streamable s => Parser s Void ()
skipOne = modify dropOne

{- | Extract the first @'ElementOf' s@ from the streamable but without consuming input.

=== __Examples:__

>>> parse peek "0123"
Right (Just '0',"0123")

>>> parse peek ""
Right (Nothing,"")
-}
{-# INLINE peek #-}
peek :: Streamable s => Parser s Void (Maybe (ElementOf s))
peek = do
    c <- gets uncons
    pure $ fmap fst c

{- | Parse one @'ElementOf' s@ satisfying a predicate.

=== __Examples:__

>>> parse (satisfy ('1' /=)) (initialize "0123")
Right ('0',Counter 1 "123")

>>> parse (satisfy ('0' /=)) (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])

>>> parse (satisfy ('1' /=)) (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE satisfy #-}
satisfy
    :: HasOffset s
    => (ElementOf s -> Bool)            -- ^ @'ElementOf' s@ predicate.
    -> Parser s (ParseError (ValidationError (ElementOf s))) (ElementOf s)
satisfy p = do
    c <- first (fmap absurd) one
    if p c then pure c else throwParseError (pure c)

{- | Parse one element matching a @'ElementOf' s@.

=== __Examples:__

>>> parse (matchOne '0') (initialize "0123")
Right ('0',Counter 1 "123")

>>> parse (matchOne '1') (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])

>>> parse (matchOne '0') (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE matchOne #-}
matchOne
    :: (HasOffset s, Eq (ElementOf s))
    => ElementOf s                      -- ^ Matching @'ElementOf' s@.
    -> Parser s (ParseError (ValidationError (ElementOf s))) (ElementOf s)
matchOne x = satisfy (== x)

{- | Parse one @'ElementOf' s@ that is an element of a foldable.

=== __Examples:__

>>> parse (oneOf "01") (initialize "0123")
Right ('0',Counter 1 "123")

>>> parse (oneOf "12") (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])

>>> parse (oneOf "12") (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE oneOf #-}
oneOf
    :: (HasOffset s, Eq (ElementOf s), Foldable t)
    => t (ElementOf s)                  -- ^ Foldable of @'ElementOf' s@ to test inclusion.
    -> Parser s (ParseError (ValidationError (ElementOf s))) (ElementOf s)
oneOf xs = satisfy (`elem` xs)
