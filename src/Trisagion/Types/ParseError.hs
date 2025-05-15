{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The @ParseError@ type.
    ParseError (..),

    -- ** Getters.
    backtrace,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)

-- Libraries.
import Optics.Core (review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Types.ErrorItem (ErrorItem, TraceItem, traceItem)


{- | The 'ParseError' type. -}
data ParseError s e where
    -- | The Nil constructor.
    Nil :: ParseError s e

    -- | The Cons constructor.
    Cons :: (Typeable d, Eq d, Show d) => !(ErrorItem s e) -> [ErrorItem s d] -> ParseError s e

-- Instances.
deriving stock instance Functor (ParseError s)
deriving stock instance (Show s, Show e) => Show (ParseError s e)

instance (Eq s, Eq e) => Eq (ParseError s e) where
    {-# INLINEABLE (==) #-}
    (==) :: ParseError s e -> ParseError s e -> Bool
    (==) Nil          Nil          = True
    (==) r@(Cons d _) s@(Cons e _) = d == e && backtrace r == backtrace s
    (==) _            _            = False

{- | Monofunctoriality for 'ParseError' in the input stream type. -}
instance MonoFunctor (ParseError s e) where
    type ElementOf (ParseError s e) = s

    {-# INLINEABLE monomap #-}
    monomap :: (s -> s) -> ParseError s e -> ParseError s e
    monomap _ Nil         = Nil
    monomap f (Cons e es) = Cons (first f e) (fmap (first f) es)

instance Semigroup (ParseError s e) where
    {-# INLINE (<>) #-}
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Nil x = x
    (<>) x   _ = x

instance Monoid (ParseError s e) where
    {-# INLINE mempty #-}
    mempty :: ParseError s e
    mempty = Nil


{- | The backtrace of a 'ParseError' as a list of 'TraceItem'. -}
{-# INLINE backtrace #-}
backtrace :: ParseError s e -> [TraceItem s]
backtrace Nil         = []
backtrace (Cons _ xs) = fmap (review traceItem) xs
