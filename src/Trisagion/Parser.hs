{- |
Module: Trisagion.Parser

The @Parser@ parsing monad.
-}

module Trisagion.Parser (
    -- * The Parsing monad.
    Parser,

    -- ** Constructors.
    embed,

    -- ** Basic functions.
    run,
    eval,
    exec,
) where

-- Imports.
-- Package.
import Trisagion.Types.Result (Result, withResult)


{- | The @Parser@ monad. -}
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor


{- | Embed a parsing function in the t'Parser' monad. -}
embed :: (s -> Result s e a) -> Parser s e a
embed = Parser

{- | Run the parser on the input and return the results.

note(s):

    * The function @'run' :: 'Parser' s e a -> (s -> 'Result' s e a)@ is the inverse of 'embed'.
-}
run :: Parser s e a -> s -> Result s e a
run (Parser f) = f

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
eval :: Parser s e a -> s -> Either e a
eval p = withResult Left (\ x _ -> Right x) . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
exec :: Parser s e a -> s -> Either e s
exec p = withResult Left (\ _ s -> Right s) . run p
