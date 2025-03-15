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
-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Package.
import Trisagion.Types.Result (Result (..), withResult)


{- | The @Parser@ monad. -}
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor


{- | The 'Bifunctor' instance, providing functoriality in the error type. -}
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap g f p = embed $ bimap g f . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @pure x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

    * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Parser s e) where
    pure :: a -> Parser s e a
    pure x = embed $ \ s -> Success x s

    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = embed $ withResult Error (\ f -> withResult Error (Success . f) . run q) . run p


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
