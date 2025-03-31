{- |
Module: Trisagion.Core.Parser

The @Parser@ monad.
-}

module Trisagion.Core.Parser (
    -- * Type operators.
    (:+:),
    (:*:),

    -- * Types.
    Parser,

    -- ** Basic functions.
    parse,
    eval,
    remainder,

    -- * 'Applicative' parsers.
    value,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void)

-- Package.
import Trisagion.Types.Result (Result (..), toEither, withResult)


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:

{- | Right-associative type operator version of the @(,)@ tuple type constructor. -}
type a :*: b = (a, b)
infixr 6 :*:


{- | The parsing monad. -}
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor


{- | The 'Bifunctor' instance, providing functoriality in the error type. -}
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap g f p = Parser $ bimap g f . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @pure x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

    * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Parser s e) where
    pure :: a -> Parser s e a
    pure x = Parser $ \ s -> Success x s

    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = Parser $ \ xs ->
        case run p xs of
            Error e1     -> Error e1
            Success f ys ->
                case run q ys of
                    Error e2     -> Error e2
                    Success x zs -> Success (f x) zs

{- | The 'Monad' instance.

The bind combinator @p >>= f@ first runs @p@ and then the parser obtained by applying the monadic
function @f@ to the result.

note(s):

    * As with @p \<*\> q@, @p >>= f@ short-circuits on @p@ erroring out.
-}
instance Monad (Parser s e) where
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = Parser $ \ xs ->
        case run p xs of
            Error e      -> Error e
            Success x ys -> run (h x) ys


{- | Run the parser on the input and return the results. -}
run :: Parser s e a -> s -> Result s e a
run (Parser p) = p

{- | Parse the input and return the results. -}
parse :: Parser s e a -> s -> e :+: (a :*: s)
parse p = toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder of the input. -}
eval :: Parser s e a -> s -> e :+: a
eval p = withResult Left (\ x _ -> Right x) . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
remainder :: Parser s e a -> s -> e :+: s
remainder p =  withResult Left (\ _ xs -> Right xs). run p


{- | Embed a value in the 'Parser' monad.

The difference with 'pure' from 'Applicative' is the more precise signature.
-}
value :: a -> Parser s Void a
value x = Parser $ \ s -> Success x s
