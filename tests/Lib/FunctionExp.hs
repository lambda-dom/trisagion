module Lib.FunctionExp (
    -- * Types.
    FunctionExp (..),

    -- ** Functions.
    makeFunction,

    -- ** Functions.
    genFunctionExp,
) where

-- Imports.
-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (choice, constant, recursive, subterm2)


{- | Defunctionalize endofunctions. -}
data FunctionExp a
    = Identity                          -- ^ Identity function.
    | Constant a                        -- ^ Constant function.
    | Binary a                          -- ^ Specialize a binary function to the first element.
    | Compose (FunctionExp a) (FunctionExp a)
    deriving stock (Eq, Show)


{- | Functionalize the 'FunctionExp' value. -}
makeFunction :: (a -> a -> a) -> FunctionExp a -> a -> a
makeFunction _ Identity      = id
makeFunction _ (Constant y)  = const y
makeFunction h (Binary x)    = h x
makeFunction h (Compose f g) = makeFunction h f . makeFunction h g 


{- | Generator for 'FunctionExp'. -}
genFunctionExp :: Gen a -> Gen (FunctionExp a)
genFunctionExp gen = go
    where
        go = Gen.recursive
            Gen.choice
            [Gen.constant Identity, Constant <$> gen, Binary <$> gen]
            [Gen.subterm2 go go Compose]
