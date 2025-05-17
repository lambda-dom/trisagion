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
    isolate,
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
import Trisagion.Types.ParseError (ParseError, ValidationError, singleton)
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Parser
import Trisagion.Parsers.ParseError (validate)
import Trisagion.Parsers.Combinators (lookAhead)


{- | Run the parser and return its result along with the prefix of consumed input.

note(s):

  * Implementation implicitly relies on normality of @p@.
-}
{-# INLINE consumed #-}
consumed :: (HasOffset s, Splittable s) => Parser s e a -> Parser s e (PrefixOf s, a)
consumed p = do
    xs <- first absurd get
    x  <- p
    n  <- offset <$> first absurd get
    -- Implicitly relies on the parser @p@ being normal, so that @n - offset xs@ is positive.
    pure (fst $ splitPrefix (n - offset xs) xs, x)

{- | Run a parser isolated to a fixed size prefix of the stream.

The prefix on which the parser runs may have a size smaller than @n@ if there is not enough input
in the stream. Any unconsumed input in the prefix is silently discarded. If such behavior is
undesirable, 'Control.Monad.guard' the parser to run with an appropriate check -- see
'Trisagion.Parsers.Streamable.ensureEOI'.

=== __Examples:__

>>> parse (isolate 2 one) "0123"
Right ('0',"23")

>>> parse (isolate 2 one) "0"
Right ('0',"")

>>> parse (isolate 2 one) ""
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE isolate #-}
isolate
    :: Splittable s
    => Word                             -- ^ Prefix size.
    -> Parser (PrefixOf s) e a          -- ^ Parser to run.
    -> Parser s e a
isolate n p = do
    prefix <- first absurd $ takePrefix n
    case eval p prefix of
        Left e  -> throw e
        Right x -> pure x

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix.

=== __Examples:__

>>> parse (takeExact 2) "0123"
Right ("01","23")

>>> parse (takeExact 10) "0123"
Left (Cons (EndOfInput 10) [])
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
