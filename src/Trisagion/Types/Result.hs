{- |
Module: Trisagion.Types.Result

The result type of parsing functions.
-}

module Trisagion.Types.Result (
    -- * Types.
    Result (..),

    -- ** Elimination functions.
    withResult,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))


{- | The t'Result' type of a parsing function, isomorphic to @'Either' e (a, s)@. -}
data Result s e a
    = Error !e      -- ^ Error case.
    | Success a !s  -- ^ Success case.
    deriving stock (Eq, Show)


-- Instances.
instance Functor (Result s e) where
    {-# INLINE fmap #-}
    fmap :: (a -> b) -> Result s e a -> Result s e b
    fmap f = withResult Error (\ s x -> Success (f x) s)

instance Bifunctor (Result s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> Result s d a -> Result s e b
    bimap g f = withResult (Error . g) (\ s x -> Success (f x) s)


{- | Case analysis elimination function for the t'Result' type. -}
{-# INLINE withResult #-}
withResult :: (e -> b) -> (s -> a -> b) -> Result s e a -> b
withResult _ f (Success x s) = f s x
withResult g _ (Error e)     = g e
