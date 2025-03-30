{- |
Module: Trisagion.Core.Parser

The @Parser@ monad.
-}

module Trisagion.Core.Parser (
    -- * Types.
    Parser,

    -- ** Basic functions.
    parse,
    eval,
    remainder,
) where

-- Imports.
-- Package.
import Trisagion.Types.Result (Result, toEither, withResult)


{- | The parsing monad. -}
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor


-- Instances.

{- | Run the parser on the input and return the results. -}
run :: Parser s e a -> s -> Result s e a
run (Parser p) = p

{- | Parse the input and return the results. -}
parse :: Parser s e a -> s -> Either e (a, s)
parse p = toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder of the input. -}
eval :: Parser s e a -> s -> Either e a
eval p = withResult Left (\ x _ -> Right x) . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
remainder :: Parser s e a -> s -> Either e s
remainder p =  withResult Left (\ _ xs -> Right xs). run p
