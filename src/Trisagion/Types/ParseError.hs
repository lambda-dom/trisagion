{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The @ParseError@ type.
    ParseError (..)
) where

-- Imports.
-- Base.
import Data.Typeable (Typeable)

-- Package.
import Trisagion.Types.ErrorItem (ErrorItem)


{- | The 'ParseError' type. -}
data ParseError s e where
    -- | The Nil constructor.
    Nil :: ParseError s e

    -- | The Cons constructor.
    Cons :: (Typeable d, Eq d, Show d) => !(ErrorItem s e) -> [ErrorItem s d] -> ParseError s e

-- Instances.
deriving stock instance Functor (ParseError s)
deriving stock instance (Show s, Show e) => Show (ParseError s e)
