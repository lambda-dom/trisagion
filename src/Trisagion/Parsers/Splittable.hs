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

-- Package.
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.ParserT (ParserT, lift)


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

