{- |
Module: Trisagion.Types.Result

The return type of parsing functions.
-}

module Trisagion.Types.Result (
    -- * Types.
    Result (..),

    -- ** Isomorphisms.
    result,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Optics.Core (Iso', iso)


{- | The 'Result' type of a parsing function, isomorphic to @'Either' e (a, s)@. -}
data Result s e a
    = Error e                           -- ^ Error case.
    | Success a !s                      -- ^ Success case.
    deriving stock (Eq, Show, Functor)

-- Instances.
instance Bifunctor (Result s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> Result s d a -> Result s e b
    bimap f _ (Error e)      = Error (f e)
    bimap _ g (Success x ys) = Success (g x) ys


{- | Isomorphism @Result s e a -> Either e (a, s)@. -}
{-# INLINE result #-}
result :: Iso' (Result s e a) (Either e (a, s))
result = iso to from
    where
        to :: Result s e a -> Either e (a, s)
        to (Error e)      = Left e
        to (Success x ys) = Right (x, ys)

        from :: Either e (a, s) -> Result s e a
        from (Left e)        = Error e
        from (Right (x, ys)) = Success x ys
