{- |
Module: Trisagion.Utils.Either

Some utilities for the `Either` coproduct bifunctor.
-}

module Trisagion.Utils.Either (
    -- * Type operators.
    (:+:),

    -- * Functor combinators.
    cozip,

    -- * Functions.
    annotate,
) where


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:


{- | Dual of 'zip' for 'Either'. -}
{-# INLINE cozip #-}
cozip :: Functor f => f a :+: f b -> f (a :+: b)
cozip = either (fmap Left) (fmap Right)


{- | Make a validator from a function producing a 'Maybe'. -}
{-# INLINE annotate #-}
annotate :: (a -> e) -> (a -> Maybe b) -> a -> e :+: b
annotate e h x = maybe (Left . e $ x) Right (h x)
