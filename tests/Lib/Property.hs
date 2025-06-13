module Lib.Property (
    -- * Extensional functional equality.
    prop_function_extensional_equality,
) where

-- Imports.
-- Testing.
import Hedgehog (Gen, PropertyT, (===), forAll)


{- | Testing extensional equality of functions. -}
prop_function_extensional_equality
    :: (Monad m, Show a, Eq b, Show b)
    => (a -> b)
    -> (a -> b)
    -> Gen a
    -> PropertyT m ()
prop_function_extensional_equality f g gen = do
    x <- forAll gen
    f x === g x
