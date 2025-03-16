{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * Types.
    ParseError,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))


{- | The @ParseError@ type. -}
data ParseError s e
    = Fail
    | ParseError !s !e
    deriving stock (Eq, Show, Functor)


-- Instances.
instance Bifunctor ParseError where
    bimap :: (s -> t) -> (d -> e) -> ParseError s d -> ParseError t e
    bimap _ _ Fail             = Fail
    bimap f g (ParseError s e) = ParseError (f s) (g e)

instance Semigroup (ParseError s e) where
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Fail x = x
    (<>) x    _ = x

instance Monoid (ParseError s e) where
    mempty :: ParseError s e
    mempty = Fail
