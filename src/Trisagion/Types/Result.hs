{- |
Module: Trisagion.Types.Result

The return type of parsing functions.
-}

module Trisagion.Types.Result (
    -- * Types.
    Result (..),

    -- ** Elimination functions.
    withResult,

    -- ** Isomorphisms.
    toEither,
    fromEither,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))


{- | The 'Result' type of a parsing function, isomorphic to @'Either' e (a, s)@. -}
data Result s e a
    = Error !e                          -- ^ Error case.
    | Success a !s                      -- ^ Success case.
    deriving stock (Eq, Show, Functor)

-- Instances.
instance Bifunctor (Result s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> Result s d a -> Result s e b
    bimap g f = withResult (Error . g) (Success . f)


{- | Case analysis elimination function for the 'Result' type. -}
{-# INLINE withResult #-}
withResult :: (e -> b) -> (a -> s -> b) -> Result s e a -> b
withResult _ f (Success x s) = f x s
withResult g _ (Error e)     = g e

{- | Forward isomorphism @'Result' s e a -> 'Either' e (a, s)@. -}
{-# INLINE toEither #-}
toEither :: Result s e a -> Either e (a, s)
toEither = withResult Left (curry Right)

{- | Inverse isomorphism of 'toEither'. -}
{-# INLINE fromEither #-}
fromEither :: Either e (a, s) -> Result s e a
fromEither = either Error (uncurry Success)
