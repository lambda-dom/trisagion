{- |
Module: Trisagion.Types.Result

The return type of parsing functions.
-}

module Trisagion.Types.Result (
    -- * Type operators.
    (:+:),
) where


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:
