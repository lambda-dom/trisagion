module Lib.Property (
    -- * Properties.
    -- ** Extensional functional equality.
    prop_function_extensional_equality,

    -- * Property groups.
    makeGroup,

    -- * Combine testing groups.
    andM,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.String (IsString (..))

-- Testing.
import Hedgehog (Gen, PropertyT, Property, Group (..), (===), forAll)


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

{- | Constructor for a property group. -}
makeGroup :: String -> [(String, Property)] -> Group
makeGroup name props = Group {
    groupName = fromString name,
    groupProperties = fmap (first fromString) props
    }

{- | Combine checking of test groups. -}
andM :: Monad m => [m Bool] -> m Bool
andM []       = pure True
andM (x : xs) = do
    b <- x
    if b then andM xs else pure False
