{- |
Module: Trisagion.Types.Result

The result type of parsing functions.
-}

module Trisagion.Types.Result (
    -- * The 'Result' type.
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


{- | The t'Result' type of a parsing function, isomorphic to @'Either' e (a, s)@. -}
data Result s e a
    = Error !e                          -- ^ Error case.
    | Success a !s                      -- ^ Success case.
    deriving stock (Eq, Show, Functor)


-- Instances.
instance Bifunctor (Result s) where
    bimap :: (d -> e) -> (a -> b) -> Result s d a -> Result s e b
    bimap g f = withResult (Error . g) (Success . f)


{- | Case analysis elimination function for the t'Result' type. -}
withResult :: (e -> b) -> (a -> s -> b) -> Result s e a -> b
withResult _ f (Success x s) = f x s
withResult g _ (Error e)     = g e


{- | Forward isomorphism with @'Either' e (a, s)@. -}
toEither :: Result s e a -> Either e (a, s)
toEither = withResult Left (curry Right)

{- | Inverse isomorphism of 'toEither'. -}
fromEither :: Either e (a, s) -> Result s e a
fromEither = either Error (uncurry Success)
