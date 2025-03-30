{- |
Module: Trisagion.Parser

The @Parser@ monad.
-}

module Trisagion.Parser (
    -- * Types.
    Parser,

    -- ** Basic functions.
    run,
    eval,
    remainder,
) where


{- | The parsing monad. -}
newtype Parser s e a = Parser (s -> Either e (a, s))
    deriving stock Functor


{- | Run the parser on the input and return the results. -}
run :: Parser s e a -> s -> Either e (a, s)
run (Parser f) = f

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
eval :: Parser s e a -> s -> Either e a
eval p = fmap fst . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
remainder :: Parser s e a -> s -> Either e s
remainder p = fmap snd . run p
