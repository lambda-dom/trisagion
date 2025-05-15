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
import Data.Typeable (Typeable)

-- Libraries.
import Optics.Core (review)

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


{- | The backtrace of a 'ParseError' as a list of 'TraceItem'. -}
{-# INLINE backtrace #-}
backtrace :: ParseError s e -> [TraceItem s]
backtrace Nil         = []
backtrace (Cons _ xs) = fmap (review traceItem) xs
