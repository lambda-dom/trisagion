{- |
Module: Trisagion.Getters.Splittable

Parsers with @Splittable s@ constraints on the state @s@.
-}

module Trisagion.Getters.Splittable (
    -- * Isolating parsers.
    isolateWith,

    -- * Parsers with @'Splittable' s@ constraint.
    remainder,
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
import Control.Monad.State (gets)
import Data.MonoTraversable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (ElementOf)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Get (Get, skip, eval, get, put)
import Trisagion.Getters.ParseError (ValidationError (..), validate, throwParseError)
import Trisagion.Getters.Streamable (InputError (..), MatchError (..), )


{- | Run a parser isolated to a prefix of the stream.

Any unconsumed input in the prefix is silently discarded. If such behavior is undesirable, guard the
parser to run with an appropriate check -- see 'Trisagion.Getters.ParseError.guardWith'.
-}
isolateWith
    :: (s -> Maybe (s, s))          -- ^ Stream splitter. The @'Nothing'@ case signals insufficient input.
    -> Get s e a                    -- ^ Parser to run.
    -> Get s (ParseError s (Either InputError e)) a
isolateWith h p = do
    xs <- get
    case h xs of
        Nothing               -> absurd <$> throwParseError xs (Left InsufficientInputError)
        Just (prefix, suffix) ->
            case eval p prefix of
                Left e -> absurd <$> throwParseError prefix (Right e)
                Right x -> put suffix $> x


{- | Get the rest of the input stream as a prefix. -}
{-# INLINE remainder #-}
remainder :: Splittable s => Get s Void (PrefixOf s)
remainder = do
    (prefix, suffix) <- gets getRemainder
    put suffix $> prefix

{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.
-}
{-# INLINE takePrefix #-}
takePrefix :: Splittable s => Word -> Get s Void (PrefixOf s)
takePrefix n = do
    (prefix, suffix) <- gets $ getAt n
    put suffix $> prefix

{- | Drop a fixed size prefix from the stream. -}
{-# INLINE dropPrefix #-}
dropPrefix :: Splittable s => Word -> Get s Void ()
dropPrefix = skip . takePrefix

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix, which is @O(n)@ for some types
    (e.g. @Text@).
-}
{-# INLINE takeExact #-}
takeExact
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => Word -> Get s (ParseError s InputError) (PrefixOf s)
takeExact n = first (fmap (either absurd id)) $ validate v (first absurd $ takePrefix n)
    where
        v prefix =
            if olength prefix < fromIntegral n
                then Left $ InputError n
                else Right prefix

{- | Parse a matching prefix.

note(s):

    * The implementation requires computing the length of the prefix, which is @O(n)@ for some types
    (e.g. @Text@).
-}
{-# INLINE match #-}
match
    :: (Splittable s, MonoFoldable (PrefixOf s), Eq (PrefixOf s))
    => PrefixOf s -> Get s (ParseError s (Either InputError (MatchError (PrefixOf s)))) (PrefixOf s)
match xs = validate v (takeExact (fromIntegral $ olength xs))
    where
        v prefix =
            if xs == prefix
                then Right prefix
                else Left $ MatchError xs

{- | Parse the longest prefix whose elements satisfy a predicate. -}
{-# INLINE takeWith #-}
takeWith :: Splittable s => (ElementOf s -> Bool) -> Get s Void (PrefixOf s)
takeWith p = do
    (prefix, suffix) <- gets $ getWith p
    put suffix $> prefix

{- | Drop the longest prefix whose elements satisfy a predicate. -}
{-# INLINE dropWith #-}
dropWith :: Splittable s => (ElementOf s -> Bool) -> Get s Void ()
dropWith = skip . takeWith

{- | Parse the longest prefix with at least one element whose elements satisfy a predicate. -}
{-# INLINE atLeastOneWith #-}
atLeastOneWith
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => (ElementOf s -> Bool) -> Get s (ParseError s (Either InputError ValidationError)) (PrefixOf s)
atLeastOneWith p = do
    s <- get
    xs <- first absurd $ takeWith p
    if not (onull xs)
        then pure xs
        else do
            b <- gets onull
            -- Either not enough input or malformed one.
            if b
                then absurd <$> throwParseError s (Left InsufficientInputError)
                else absurd <$> throwParseError s (Right ValidationError)
