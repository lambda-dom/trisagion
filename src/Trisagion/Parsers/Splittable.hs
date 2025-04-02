{- |
Module: Trisagion.Parsers.Splittable

Parsers with @'Splittable' s@ constraints on the input stream @s@.

Implementations requiring the computation of the length of a prefix are duly noted. This can be
important for performance because for `Splittable` like @Text@ this is @O(n)@, while for other types
like lazy bytestrings, it forces the entirety of the value into memory which is probably not what
is desired.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers with a @'Splittable' s => 'Parser' s e a@ constraint.
    dropPrefix,
    takeExact,
    dropWith,
    match,
    atLeastOneWith,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void, absurd)

-- non-Hackage libraries.
import Data.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser (Parser, ParserPE, InputError (..), (:+:), takePrefix, takePrefixWith)
import Trisagion.Parsers.Combinators (skip)
import Trisagion.Parsers.ParseError (ValidationError (..), validate, capture, throwParseError)
import Data.MonoFunctor (MonoFunctor(..))
import Trisagion.Parsers.Streamable (eoi)


{- | Drop a fixed size prefix from the stream. -}
dropPrefix :: Splittable s => Word -> Parser s Void ()
dropPrefix = skip . takePrefix

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix.
-}
takeExact
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s))
    => Word -> ParserPE s InputError (PrefixOf s)
takeExact n = first (fmap (either absurd id)) $ validate v (first absurd $ takePrefix n)
    where
        v prefix =
            if monolength prefix /= n
                then Left $ InputError n
                else Right prefix

{- | Drop the longest prefix whose elements satisfy a predicate. -}
dropWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void ()
dropWith = skip . takePrefixWith

{- | Parse a matching prefix.

note(s):

    * The implementation requires computing the length of the argument prefix.
-}
match
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s), Eq (PrefixOf s))
    => PrefixOf s                       -- ^ Matching prefix.
    -> ParserPE s (InputError :+: ValidationError (PrefixOf s)) (PrefixOf s)
match xs = validate v (takeExact (monolength xs))
    where
        v prefix =
            if xs == prefix
                then Right prefix
                else Left $ ValidationError prefix

{- | Parse the longest prefix with at least one element whose elements satisfy a predicate. -}
atLeastOneWith
    :: (HasPosition s, Splittable s, MonoFoldable (PrefixOf s))
    => (ElementOf s -> Bool)            -- ^ Predicate on @'ElementOf' s@.
    -> ParserPE s (InputError :+: ValidationError (PrefixOf s)) (PrefixOf s)
atLeastOneWith p = capture $ do
    xs <- first absurd $ takePrefixWith p
    if not (mononull xs)
        then pure xs
        else do
            -- Either not enough input or malformed one.
            b <- first absurd eoi
            if b
                then absurd <$> throwParseError (Left InsufficientInputError)
                else absurd <$> throwParseError (Right $ ValidationError xs)
