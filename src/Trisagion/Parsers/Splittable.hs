{- |
Module: Trisagion.Parsers.Splittable

Parsers with @Splittable m a b s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers.
    take,
    drop,
    takeWith,
    dropWith,
    takeExact,
    takeWith1,
    isolate,
    match,
    consumed,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (take, drop)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState(..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Types.Result ((:+:), Result (..))
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.ParserT (ParserT, parse, embed, lift, lookAhead, throw)
import Trisagion.Parsers.Streamable (ValidationError (..), InputError (..), satisfy)


{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.
-}
{-# INLINE take #-}
take :: Splittable m a b s => Word -> ParserT m s Void b
take n = do
    (prefix, remainder) <- first absurd $ lift (splitAtM n)
    put remainder $> prefix

{- | Drop a fixed size prefix from the stream. -}
{-# INLINE drop #-}
drop :: Splittable m a b s => Word -> ParserT m s Void ()
drop n = do
    (_, remainder) <- first absurd $ lift (splitAtM n)
    put remainder $> ()

{- | Parse the longest prefix whose elements satisfy a predicate. -}
{-# INLINE takeWith #-}
takeWith :: Splittable m a b s => (a -> Bool) -> ParserT m s Void b
takeWith p = do
    (prefix, remainder) <- first absurd $ lift (splitWithM p)
    put remainder $> prefix

{- | Parse the longest prefix whose elements satisfy a predicate. -}
{-# INLINE dropWith #-}
dropWith :: Splittable m a b s => (a -> Bool) -> ParserT m s Void ()
dropWith p = do
    (_, remainder) <- first absurd $ lift (splitWithM p)
    put remainder $> ()

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix.
-}
{-# INLINE takeExact #-}
takeExact
    :: (HasOffset m s, Splittable m a b s, MonoFoldable a b)
    => Word -> ParserT m s (ParseError InputError) b
takeExact n = do
    m <- first absurd $ lift offset
    prefix <- first absurd $ take n
    if monolength prefix /= n
        then throw $ ParseError m (InputError n)
        else pure prefix

{- | Parse a matching prefix.

note(s):

    * Implementation requires computing the length of the argument prefix.
-}
{-# INLINE match #-}
match
    :: (HasOffset m s, Splittable m a b s, Eq b, MonoFoldable a b)
    => b                                -- ^ Matching prefix.
    -> ParserT m s (ParseError ((ValidationError b) :+: InputError)) b
match xs = do
    m <- first absurd $ lift offset
    prefix <- first (fmap Right) $ takeExact (monolength xs)
    if xs == prefix
        then pure xs
        else throw $ ParseError m (Left (ValidationError xs))

{- | Parse the longest prefix with at least one element, whose elements satisfy a predicate. -}
{-# INLINE takeWith1 #-}
takeWith1
    :: (Splittable m a b s, HasOffset m s)
    => (a -> Bool)                      -- ^ Predicate on @a@.
    -> ParserT m s (ParseError (ValidationError a :+: InputError)) b
takeWith1 p = do
    x <- first absurd $ lookAhead (satisfy p)
    case x of
        Left e  -> throw e
        Right _ -> first absurd $ takeWith p

{- | Run a parser isolated to a fixed size prefix of the stream.

The prefix on which the parser runs may have a size smaller than @n@ if there is not enough input
in the stream. Any unconsumed input in the prefix is returned along with the result.
-}
{-# INLINE isolate #-}
isolate
    :: Splittable m a b s
    => Word                             -- ^ Prefix size.
    -> ParserT m b e a                  -- ^ Parser to run.
    -> ParserT m s e (a, b)
isolate n p = embed $ \xs -> do
    (prefix, remainder) <- splitAtM n xs
    r      <- parse p prefix
    case r of
        Left e  -> pure $ Error e
        Right x -> pure $ Success x remainder

{- | Run the parser and return its result along with the prefix of consumed input.

note(s):

  * Implementation requires computing the difference of offsets, so it implicitly relies on
    normality of @p@.
-}
{-# INLINE consumed #-}
consumed :: (HasOffset m s, Splittable m a b s) => ParserT m s e a -> ParserT m s e (b, a)
consumed p = do
    start  <- first absurd $ lift offset
    xs <- get
    x  <- p
    end  <- first absurd $ lift offset
    -- Implicitly relies on the parser @p@ being normal, for positivity of @start - end@.
    ys <- first absurd $ lift (const (fst <$> splitAtM (start - end) xs))
    pure (ys, x)
