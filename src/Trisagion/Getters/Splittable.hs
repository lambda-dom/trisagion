{- |
Module: Trisagion.Getters.Splittable

Parsers with @Splittable s@ constraints on the state @s@.
-}

module Trisagion.Getters.Splittable (
    -- * Isolating parsers.
    isolateWith,

    -- * Parsers with @'Splittable' s@ constraint.
    takePrefix,
    dropPrefix,
    takeExact,
    match,
    takeWith,
    dropWith,
    atLeastOneWith,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Data.MonoTraversable (MonoFoldable (..), Element)

-- Package.
import Trisagion.Types.ParseError (ParseError, makeParseError)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Get (Get, eval)
import Trisagion.Getters.Streamable (InputError (..), MatchError (..), ValidationError (..))
import Trisagion.Getters.Combinators (skip, validate)


{- | Run a parser isolated to a prefix of the stream.

Any unconsumed input in the prefix is silently discarded.
-}
isolateWith
    :: (s -> Maybe (s, s))      -- ^ Stream splitter. The @'Nothing'@ case signals insufficient input.
    -> Get s e a                -- ^ Parser to run.
    -> Get s (ParseError s (Either InputError e)) a
isolateWith h p = do
    xs <- get
    case h xs of
        Nothing               -> throwError $ makeParseError xs (Left InsufficientInputError)
        Just (prefix, suffix) ->
            case eval p prefix of
                Left e -> throwError $ makeParseError prefix (Right e)
                Right x -> put suffix $> x

{- | Get a fixed size prefix from the stream.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.
-}
takePrefix :: Splittable s => Word -> Get s Void (PrefixOf s)
takePrefix n = do
    (prefix, suffix) <- gets $ getAt n
    put suffix $> prefix

{- | Drop a fixed size prefix from the stream. -}
dropPrefix :: Splittable s => Word -> Get s Void ()
dropPrefix = skip . takePrefix

{- | Get an exact, fixed size prefix from the stream.

note(s):

    * The implementation requires computing the length of the prefix, which is @O(n)@ for some types
    (e.g. @Text@).
-}
takeExact
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => Word -> Get s (ParseError s InputError) (PrefixOf s)
takeExact n = first (fmap (either absurd id)) $ validate v (first absurd $ takePrefix n)
    where
        v prefix =
            if olength prefix < fromIntegral n
                then Left $ InputError n
                else Right prefix

{- | Get a matching prefix from the stream.

note(s):

    * The implementation requires computing the length of the prefix, which is @O(n)@ for some types
    (e.g. @Text@).
-}
match
    :: (Splittable s, MonoFoldable (PrefixOf s), Eq (PrefixOf s))
    => PrefixOf s -> Get s (ParseError s (Either InputError (MatchError (PrefixOf s)))) (PrefixOf s)
match xs = validate v (takeExact (fromIntegral $ olength xs))
    where
        v prefix =
            if xs == prefix
                then Right prefix
                else Left $ MatchError xs

{- | Get the longest prefix from the streamable whose elements satisfy a predicate. -}
takeWith :: Splittable s => (Element s -> Bool) -> Get s Void (PrefixOf s)
takeWith p = do
    (prefix, suffix) <- gets $ getWith p
    put suffix $> prefix

{- | Drop the longest prefix from the stream whose elements satisfy a predicate. -}
dropWith :: Splittable s => (Element s -> Bool) -> Get s Void ()
dropWith = skip . takeWith

{- | Get the longest prefix with at least one element whose elements satisfy a predicate. -}
atLeastOneWith
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => (Element s -> Bool) -> Get s (ParseError s (Either InputError ValidationError)) (PrefixOf s)
atLeastOneWith p = do
    s <- get
    xs <- first absurd $ takeWith p
    if not (onull xs)
        then pure xs
        else do
            b <- gets onull
            -- Either not enough input or malformed one.
            if b
                then throwError $ makeParseError s (Left InsufficientInputError)
                else throwError $ makeParseError s (Right ValidationError)
