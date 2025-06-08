{- |
Module: Trisagion.Parsers.Splittable

Parsers wth @'Splittable' s@ constraints.

Implementations requiring the computation of the length of a prefix have a
@'Mono.Typeclasses.MonoFoldable.MonoFoldable' s@ constraint. This can be important for performance
because for a `Splittable` like @Text@ this is @O(n)@, while for other types like lazy bytestrings,
it forces the entirety of the value into memory which is probably not what is desired.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers @'Splittable' s => 'Parser' s e a@.
    takePrefix,
    skipPrefix,
    takeExact,
    matchPrefix,
    takeWith,
    takeWith1,
    skipWith,
    takeRemainder,
    isolate,
    consumed,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), gets, modify)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError, ValidationError, makeEOI)
import Trisagion.Parser (Parser, eval)
import Trisagion.Parsers.Combinators (lookAhead)
import Trisagion.Parsers.ParseError (throwParseError)
import Trisagion.Parsers.Streamable (InputError, satisfy)


-- $setup
-- >>> import Trisagion.Streams.Counter
-- >>> import Trisagion.Parser
-- >>> import Trisagion.Parsers.Streamable


{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.

=== __Examples:__

>>> parse (takePrefix 2) "0123"
Right ("01","23")

>>> parse (takePrefix 10) "0123"
Right ("0123","")
-}
{-# INLINE takePrefix #-}
takePrefix :: Splittable s => Word -> Parser s Void (PrefixOf s)
takePrefix n = do
    (prefix, rest) <- gets (splitPrefix n)
    put rest $> prefix

{- | Drop a fixed size prefix from the stream.

=== __Examples:__

>>> parse (skipPrefix 2) "0123"
Right ((),"23")

>>> parse (skipPrefix 10) "0123"
Right ((),"")
-}
{-# INLINE skipPrefix #-}
skipPrefix :: Splittable s => Word -> Parser s Void ()
skipPrefix n = modify (dropPrefix n)

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
        then throwError $ makeEOI n
        else pure prefix

{- | Parse a matching prefix.

note(s):

    * Implementation requires computing the length of the argument prefix.

=== __Examples:__

>>> parse (matchPrefix "01") (initialize "0123")
Right ("01",Counter 2 "23")

>>> parse (matchPrefix "012345") (initialize "0123")
Left (Cons (EndOfInput 6) [])

>>> parse (matchPrefix "{}") (initialize "0123")
Left (Cons (ErrorItem 2 (ValidationError "{}")) [])
-}
{-# INLINE matchPrefix #-}
matchPrefix
    :: (HasOffset s, Splittable s, Eq (PrefixOf s), MonoFoldable (PrefixOf s))
    => PrefixOf s                       -- ^ Matching prefix.
    -> Parser s (ParseError (ValidationError (PrefixOf s))) (PrefixOf s)
matchPrefix xs = do
    prefix <- first (fmap absurd) $ takeExact (monolength xs)
    if xs == prefix then pure xs else throwParseError (pure xs)

{- | Parse the longest prefix whose elements satisfy a predicate.

=== __Examples:__

>>> parse (takeWith ('3' /=)) "0123"
Right ("012","3")

>>> parse (takeWith ('3' /=)) ""
Right ("","")
-}
{-# INLINE takeWith #-}
takeWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void (PrefixOf s)
takeWith p = do
    (prefix, rest) <- gets (splitWith p)
    put rest $> prefix

{- | Parse the longest prefix with at least one element whose elements satisfy a predicate.

=== __Examples:__

>>> parse (takeWith1 ('0' ==)) (initialize "0123")
Right ("0",Counter 1 "123")

>>> parse (takeWith1 ('0' ==)) (initialize "0003")
Right ("000",Counter 3 "3")

>>> parse (takeWith1 ('1' ==)) (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])

>>> parse (takeWith1 ('0' ==)) (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE takeWith1 #-}
takeWith1
    :: (HasOffset s, Splittable s)
    => (ElementOf s -> Bool)            -- ^ Predicate on @'ElementOf' s@.
    -> Parser s (ParseError (ValidationError (ElementOf s))) (PrefixOf s)
takeWith1 p = do
    x <- first absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throwError e
        Right _ -> first absurd $ takeWith p

{- | Drop the longest prefix whose elements satisfy a predicate.

=== __Examples:__

>>> parse (skipWith ('3' /=)) "0123"
Right ((),"3")

>>> parse (skipWith ('3' /=)) ""
Right ((),"")
-}
{-# INLINE skipWith #-}
skipWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void ()
skipWith p = modify (dropWith p)

{- | Parse the remainder of the stream as a prefix. -}
{-# INLINE takeRemainder #-}
takeRemainder :: Splittable s => Parser s Void (PrefixOf s)
takeRemainder = do
    (prefix, rest) <- gets splitRemainder
    put rest $> prefix

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
        Left e  -> throwError e
        Right x -> pure x

{- | Run the parser and return its result along with the prefix of consumed input.

note(s):

  * Implementation requires computing the difference of offsets, so it implicitly relies on
    normality of @p@.

=== __Examples:__

>>> parse (consumed one) (initialize "0123")
Right (("0",'0'),Counter 1 "123")

>>> parse (consumed one) (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE consumed #-}
consumed :: (HasOffset s, Splittable s) => Parser s e a -> Parser s e (PrefixOf s, a)
consumed p = do
    xs <- get
    x  <- p
    n  <- gets offset
    -- Implicitly relies on the parser @p@ being normal, for positivity of @n - offset xs@.
    pure (fst $ splitPrefix (n - offset xs) xs, x)
