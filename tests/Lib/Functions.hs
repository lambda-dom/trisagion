module Lib.Functions (
    -- * Types.
    Function (..),

    -- ** Functions.
    fromFunction,

    -- ** Generators.
    genFunction,
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Word (Word16)

-- Libraries.
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector (fromList)

-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (recursive, choice, word16, subterm2)
import qualified Hedgehog.Range as Range (constantBounded)

-- Testing helpers.
import Lib.Predicates (Predicate, fromPredicate, genPredicate)


{- | The 'Function' type. -}
data Function a b where
    Const  :: b -> Function a b
    Atomic :: Word16 -> Function a b
    If     :: Predicate a -> Function a b -> Function a b -> Function a b
    Binary :: Word16 -> Function a b -> Function a b -> Function a b
    deriving stock (Eq, Show, Ord)


{- | Functionalize a 'Function' value. -}
fromFunction
    :: forall a b . (Ord a)
    => NonEmpty (a -> b)
    -> NonEmpty (b -> b -> b)
    -> Function a b
    -> a
    -> b
fromFunction fs bs = go
    where
        fns :: Vector (a -> b)
        fns = Vector.fromList (toList fs)

        bns :: Vector (b -> b -> b)
        bns = Vector.fromList (toList bs)

        go :: Function a b -> a -> b
        go r = case r of
            Const  y     -> const y
            Atomic i     ->
                case fns !? (fromIntegral i `rem` length fns) of
                    Just f -> f
                    -- Unreachable.
                    Nothing -> error "Index out of bounds."
            If     p f g -> \ x -> if fromPredicate p x then go f x else go g x
            Binary i f g ->
                case bns !? (fromIntegral i `rem` length fns) of
                    Just h  -> \ x -> h (go f x) (go g x)
                    -- Unreachable.
                    Nothing -> error "Index out of bounds."

{- | Generator for 'Function' values. -}
genFunction :: Ord a => Gen a -> Gen b -> Gen (Function a b)
genFunction g1 g2 = go
    where
        gen = Gen.word16 Range.constantBounded
        go = Gen.recursive
            Gen.choice
            [Const <$> g2, Atomic <$> gen]
            [
                genPredicate g1 >>= \ p -> Gen.subterm2 go go (If p),
                gen >>= \ i -> Gen.subterm2 go go (Binary i)
            ]
