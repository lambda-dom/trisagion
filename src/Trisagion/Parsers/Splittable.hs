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
    consumed,
    takeExact,
    takeWith1,
    match,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (absurd)

-- Libraries.
import Optics.Core ((%), review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Types.ErrorItem (endOfInput)
import Trisagion.Types.ParseError (ParseError, singleton)
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser (Parser, InputError, takePrefix, takeWith, throw, get)
import Trisagion.Parsers.ParseError (ValidationError, validate)
import Trisagion.Parsers.Streamable (satisfy)
import Trisagion.Parsers.Combinators (lookAhead)


{- | Run the parser and return its result along with the prefix of consumed input. -}
{-# INLINE consumed #-}
consumed :: (HasOffset s, Splittable s) => Parser s e a -> Parser s e (PrefixOf s, a)
consumed p = do
    xs <- first absurd get
    x  <- p
    n  <- offset <$> first absurd get
    let size = n - offset xs
    pure (fst $ splitPrefix size xs, x)

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix.
-}
{-# INLINE takeExact #-}
takeExact
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => Word -> Parser s InputError (PrefixOf s)
takeExact n = do
    prefix <- first absurd $ takePrefix n
    if monolength prefix /= n
        then throw $ review (singleton % endOfInput) n
        else pure prefix

{- | Parse the longest prefix with at least one element whose elements satisfy a predicate. -}
{-# INLINE takeWith1 #-}
takeWith1
    :: (HasOffset s, Splittable s)
    => (ElementOf s -> Bool)            -- ^ Predicate on @'ElementOf' s@.
    -> Parser s (ParseError (ValidationError (ElementOf s))) (PrefixOf s)
takeWith1 p = do
    x <- first absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throw e
        Right _ -> first absurd $ takeWith p

{- | Parse a matching prefix.

note(s):

    * The implementation requires computing the length of the argument prefix.
-}
{-# INLINE match #-}
match
    :: (HasOffset s, Splittable s, Eq (PrefixOf s), MonoFoldable (PrefixOf s))
    => PrefixOf s                       -- ^ Matching prefix.
    -> Parser s (ParseError (ValidationError (PrefixOf s))) (PrefixOf s)
match xs = first (fmap (either id absurd)) $ validate v (takeExact (monolength xs))
    where
        v prefix = if xs == prefix then Right prefix else Left $ pure prefix
