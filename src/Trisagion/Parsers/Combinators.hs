{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * Parsers without errors.
    value,
    observe,
    lookAhead,
    maybe,

    -- * 'Functor' parsers.
    skip,

    -- * 'Applicative' parsers.
    before,
    after,
    between,
    zip,
    zipWith,
    repeat,
    sequence,
) where

-- Imports.
-- Prelude.
import Prelude hiding (either, maybe, repeat, sequence, zip, zipWith)
import Prelude as Base (either)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Parser (Parser, catchErrorWith, eval)


{- | Embed a value in the 'Parser' monad.

The difference with 'pure' from 'Applicative' is the more precise signature.
-}
value :: a -> Parser s Void a
value = pure

{- | Run the parser and return the result as a 'Right'; on error, backtrack and return it as a 'Left'. -}
observe :: Parser s e a -> Parser s Void (Either e a)
observe p = do
    s <- get
    catchErrorWith
        (Right <$> p)
        (\ e -> put s $> Left e)

{- | Run the parser and return the result, but do not consume any input. -}
lookAhead :: Parser s e a -> Parser s Void (Either e a)
lookAhead p = eval p <$> first absurd get

{- | Run the parser and return the result as a @'Just'@. If it errors, backtrack and return @'Nothing'@.

The difference with @'Control.Applicative.optional'@ is the more precise type signature.
-}
maybe :: Parser s e a -> Parser s Void (Maybe a)
maybe p = Base.either (const Nothing) Just <$> observe p


{- | Run the parser but discard the result.

note(s):

    * Various combinators have more efficient @skip*@ versions that avoid constructing intermediate
    values; use those whenever possible.
-}
skip :: Parser s e a -> Parser s e ()
skip = ($> ())


{- | The parser @'before' b p@ parses @b@ and @p@ in succession, returning the result of @p@. -}
before
    :: Parser s e b                     -- ^ Parser to run first.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s e a
before = (*>)

{- | The parser @'after' a p@ parses @p@ and @a@ in succession, returning the result of @p@. -}
after
    :: Parser s e b                     -- ^ Parser to run after.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s e a
after = flip (<*)

{- | The parser @'between' o c p@ parses @o@, @p@ and @c@ in succession, returning the result of @p@. -}
between
    :: Parser s e b                     -- ^ Opening parser.
    -> Parser s e c                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run in-between.
    -> Parser s e a
between open close = before open . after close

{- | Sequence two parsers and zip the results in a pair. -}
zip :: Parser s e a -> Parser s e b -> Parser s e (a, b)
zip = zipWith (,)

{- | Sequence two parsers and zip the results with a binary function. -}
zipWith :: (a -> b -> c) -> Parser s e a -> Parser s e b -> Parser s e c
zipWith f p q = f <$> p <*> q

{- | Run the parser @n@ times and return the list of results.

It is guaranteed that the list of results has exactly @n@ elements.
-}
repeat :: Word -> Parser s e a -> Parser s e [a]
repeat n p = go n
    where
        go 0 = pure []
        go m = (:) <$> p <*> go (pred m)

{- | Sequence a traversable of parsers and return the traversable of results. -}
sequence :: Traversable t => t (Parser s e a) -> Parser s e (t a)
sequence = sequenceA

