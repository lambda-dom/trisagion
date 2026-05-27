{- |
Module: Trisagion.Parser

The @Parser@ monad transformer.
-}

module Trisagion.Parser (
    -- * The parsing monad.
    Parser,

    -- * Basic functions.
    embed,
    run,
    parse,
    eval,
    remainder,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Types.Result (Result (..), toEither)


{- | The parsing monad @Parser s e a@.

@s@ is the input stream type, @e@ is the type of parsing errors that the parser can throw and @a@
is the type of parsed values.
-}
type Parser ::  Type -> Type -> Type -> Type
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor


-- Instances.
{- | Provides functoriality on the error type. -}
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap f g (Parser h) = embed $ bimap f g . h


{- | Embed a parsing function in the t'Parser' monad. -}
{-# INLINE embed #-}
embed ::  (s ->  Result s e a) -> Parser s e a
embed = Parser

{- | Run the parser on the input and return the results.

The inverse of @'embed'@.
-}
{-# INLINE run #-}
run :: Parser s e a -> s ->  Result s e a
run (Parser f) = f

{- | Parse the input and return the result as an 'Either'. -}
{-# INLINE parse #-}
parse :: Parser s e a -> s -> e :+: (a, s)
parse p = toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Parser s e a -> s -> e :+: a
eval p = fmap fst . parse p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Parser s e a -> s -> e :+: s
remainder p = fmap snd . parse p
