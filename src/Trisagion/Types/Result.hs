{- |
Module: Trisagion.Types.Result

The return type of parsing functions.
-}

module Trisagion.Types.Result (
    -- * Types.
    Result (..),

    -- ** Isomorphisms.
    toEither,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)

-- Package.
import Trisagion.Utils.Either ((:+:))
import Optics.Core (Iso', iso)


{- | The 'Result' type of a parsing function, isomorphic to @'Either' e (a, s)@. -}
type Result :: Type -> Type -> Type -> Type
data Result s e a
    = Error !e                          -- ^ Error case.
    | Success a !s                      -- ^ Success case.
    deriving stock (Eq, Show, Functor)


-- Instances.
instance Bifunctor (Result s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> Result s d a -> Result s e b
    bimap f _ (Error e)      = Error (f e)
    bimap _ g (Success x xs) = Success (g x) xs


{- | The isomorphism @'Result' s e a -> 'Either' e (a, s)@. -}
{-# INLINE toEither #-}
toEither :: Iso' (Result s e a) (e :+: (a, s))
toEither = iso to from
    where
        to :: Result s e a -> e :+: (a, s)
        to (Error e)      = Left e
        to (Success x xs) = Right (x, xs)

        from :: e :+: (a, s) -> Result s e a
        from = either Error (uncurry Success)
