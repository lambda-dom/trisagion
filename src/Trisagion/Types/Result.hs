{- |
Module: Trisagion.Types.Result

The return type of parsing functions.
-}

module Trisagion.Types.Result (
    -- * Type operators.
    (:+:),

    -- * Types.
    Result (..),

    -- ** Constructor helpers.
    errorM,
    successM,

    -- ** Isomorphisms.
    toEither,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:


{- | The 'Result' type of a parsing function, isomorphic to @'Either' e (a, s)@. -}
type Result :: Type -> Type -> Type -> Type
data Result s e a
    = Error e                           -- ^ Error case.
    | Success a !s                      -- ^ Success case.
    deriving stock (Eq, Show, Functor)


-- Instances.
instance Bifunctor (Result s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> Result s d a -> Result s e b
    bimap f _ (Error e)      = Error (f e)
    bimap _ g (Success x xs) = Success (g x) xs


{- | Constructor helper for 'Result'. -}
{-# INLINE errorM #-}
errorM :: Applicative m => e -> m (Result s e a)
errorM = pure . Error

{- | Constructor helper for 'Result'. -}
{-# INLINE successM #-}
successM :: Applicative m => a -> s -> m (Result s e a)
successM x = pure . Success x


{- | The isomorphism @'Result' s e a -> 'Either' e (a, s)@. -}
{-# INLINE toEither #-}
toEither :: Result s e a -> e :+: (a, s)
toEither (Error e)      = Left e
toEither (Success x xs) = Right (x, xs)
