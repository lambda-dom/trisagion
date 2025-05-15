{- |
Module: Trisagion.Parsers.Splittable

Parsers with @'Splittable' s@ constraints on the input stream @s@.

Implementations requiring the computation of the length of a prefix have a @'MonoFoldable' s@
constraint. This can be important for performance because for a `Splittable` like @Text@ this is
@O(n)@, while for other types like lazy bytestrings, it forces the entirety of the value into
memory which is probably not what is desired.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers with a @'Splittable' s => 'Parser' s e a@ constraint.
    takeExact,
    takeWith1,
    match,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Optics.Core ((%), review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Types.ErrorItem (endOfInput, errorItem)
import Trisagion.Types.ParseError (ParseError, singleton)
import qualified Trisagion.Typeclasses.Streamable as Streamable (null)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser (Parser, takePrefix, takeWith, get)
import Trisagion.Parsers.ParseError (ValidationError, validate)


{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix.
-}
{-# INLINE takeExact #-}
takeExact
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => Word -> Parser s (ParseError s Void) (PrefixOf s)
takeExact n = do
    prefix <- first absurd $ takePrefix n
    if monolength prefix /= n
        then throwError $ review (singleton % endOfInput) n
        else pure prefix

{- | Parse the longest prefix with at least one element whose elements satisfy a predicate. -}
{-# INLINE takeWith1 #-}
takeWith1
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => (ElementOf s -> Bool)            -- ^ Predicate on @'ElementOf' s@.
    -> Parser s (ParseError s (ValidationError (PrefixOf s))) (PrefixOf s)
takeWith1 p = do
    xs <- first absurd $ takeWith p
    if not (mononull xs)
        then pure xs
        else do
            ys <- first absurd get
            -- Either not enough input or malformed one.
            if Streamable.null ys
                then throwError $ review (singleton % endOfInput) 1
                else throwError $ review (singleton % errorItem) (ys, pure xs)

{- | Parse a matching prefix.

note(s):

    * The implementation requires computing the length of the argument prefix.
-}
{-# INLINE match #-}
match
    :: (Splittable s, Eq (PrefixOf s), MonoFoldable (PrefixOf s))
    => PrefixOf s                       -- ^ Matching prefix.
    -> Parser s (ParseError s (ValidationError (PrefixOf s))) (PrefixOf s)
match xs = first (fmap (either id absurd)) $ validate v (takeExact (monolength xs))
    where
        v prefix = if xs == prefix then Right prefix else Left $ pure prefix
