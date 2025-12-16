{- |
Module: Trisagion.ParserT

The @ParserT@ monad transformer.
-}

module Trisagion.ParserT (
    -- * The parsing monad transformer.
    ParserT,
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
import Data.Functor.Identity (Identity)
import Data.Kind (Type)

-- Package.
import Trisagion.Types.Result (Result (..), (:+:), toEither)


{- | The parsing monad transformer @ParserT m s e a@.

@m@ is the underlying monad, @s@ is the input stream type, @e@ is the type of parsing errors that
the parser can throw and @a@ is the type of parsed values.
-}
type ParserT :: (Type -> Type) -> Type -> Type -> Type -> Type
newtype ParserT m s e a = ParserT (s ->  m (Result s e a))
    deriving stock Functor


{- | The 'Bifunctor' instance, providing functoriality in the error type. -}
instance Functor m => Bifunctor (ParserT m s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> ParserT m s d a -> ParserT m s e b
    bimap g f p = embed $ fmap (bimap g f) . run p


{- | Type alias for @t'ParserT' m@ specialized to @m ~ 'Identity'@.-}
type Parser :: Type -> Type -> Type -> Type
type Parser = ParserT Identity


{- | Embed a parsing function in the t'ParserT' monad. -}
{-# INLINE embed #-}
embed ::  (s ->  m (Result s e a)) -> ParserT m s e a
embed = ParserT

{- | Run the parser on the input and return the results. -}
{-# INLINE run #-}
run :: ParserT m s e a -> s ->  m (Result s e a)
run (ParserT f) = f

{- | Parse the input and return the results. -}
{-# INLINE parse #-}
parse :: Functor m => ParserT m s e a -> s -> m (e :+: (a, s))
parse p = fmap toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Functor m => ParserT m s e a -> s -> m (e :+: a)
eval p = fmap (fmap fst) . parse p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Functor m => ParserT m s e a -> s -> m (e :+: s)
remainder p =  fmap (fmap snd) . parse p
