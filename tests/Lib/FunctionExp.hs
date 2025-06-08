module Lib.FunctionExp (
    -- * Types.
    FunctionExp (..),

    -- ** Functions.
    makeFunction,
    makeParserFunctionExp,

    -- ** Generators.
    genFunctionExp,
) where

-- Imports.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (absurd)
import Data.Word (Word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Testing.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (choice, constant, recursive, subterm2)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Parser (Parser)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parsers.Streamable (one)


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

{- | Given a parser for @a@ construct a parser for @'FunctionExp' a@.

Used to generate testing parsers that parse functions.
-}
makeParserFunctionExp
    :: (Streamable s, ElementOf s ~ Word8)
    => Parser s (ParseError e) a -> Parser s (ParseError e) (FunctionExp a)
makeParserFunctionExp p = go
    where
        go = do
            tag <- (`rem` 4) <$> first (fmap absurd) one
            case tag of
                n | n == 0 -> pure Identity
                n | n == 1 -> Constant <$> p
                n | n == 2 -> Binary <$> p
                n | n == 3 -> Compose <$> go <*> go
                _          -> pure Identity
