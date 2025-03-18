{- |
Module: Trisagion.Parsers.Splittable

Parsers with @'Splittable' s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers with a @'Splittable' s => 'Parser' s e a@ constraint.
    takePrefix,
    dropPrefix,
    takeExact,
    match,
    takeWith,
    dropWith,
    atLeastOneWith,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (splitAt)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState(..), gets)

-- non-Hackage libraries.
import Data.MonoFunctor (ElementOf)
import Data.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.Streamable (isNull)
import Trisagion.Typeclasses.Splittable (Splittable(..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip)
import Trisagion.Parsers.ParseError (ValidationError (..), validate, throwParseError, capture)
import Trisagion.Parsers.Streamable (InputError (..))


{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.
-}
takePrefix :: Splittable s => Word -> Parser s Void (PrefixOf s)
takePrefix n = do
    (prefix, suffix) <- gets $ splitAt n
    put suffix $> prefix

{- | Drop a fixed size prefix from the stream. -}
dropPrefix :: Splittable s => Word -> Parser s Void ()
dropPrefix = skip . takePrefix

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix, which is @O(n)@ for some types
    (e.g. @Text@). For other types like @LazyText@, it forces the entirety of the value into memory
    which is probably not desired.
-}
takeExact
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => Word -> Parser s (ParseError s InputError) (PrefixOf s)
takeExact n = first (fmap (either absurd id)) $ validate v (first absurd $ takePrefix n)
    where
        v prefix =
            if monolength prefix /= n
                then Left $ InputError n
                else Right prefix

{- | Parse a matching prefix.

note(s):

    * The implementation requires computing the length of the prefix.
-}
match
    :: (Splittable s, MonoFoldable (PrefixOf s), Eq (PrefixOf s))
    => PrefixOf s
    -> Parser s (ParseError s (Either InputError (ValidationError (PrefixOf s)))) (PrefixOf s)
match xs = validate v (takeExact (monolength xs))
    where
        v prefix =
            if xs == prefix
                then Right prefix
                else Left $ ValidationError prefix

{- | Parse the longest prefix whose elements satisfy a predicate. -}
takeWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void (PrefixOf s)
takeWith p = do
    (prefix, suffix) <- gets $ splitWith p
    put suffix $> prefix

{- | Drop the longest prefix whose elements satisfy a predicate. -}
dropWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void ()
dropWith = skip . takeWith

{- | Parse the longest prefix with at least one element whose elements satisfy a predicate. -}
atLeastOneWith
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => (ElementOf s -> Bool)
    -> Parser s (ParseError s (Either InputError (ValidationError (PrefixOf s)))) (PrefixOf s)
atLeastOneWith p = capture $ do
    xs <- first absurd $ takeWith p
    if not (mononull xs)
        then pure xs
        else do
            -- Either not enough input or malformed one.
            b <- gets isNull
            if b
                then absurd <$> throwParseError (Left InsufficientInputError)
                else absurd <$> throwParseError (Right $ ValidationError xs)
