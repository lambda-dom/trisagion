{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Error types.
    InputError (..),

    -- * Typeclasses.
    Streamable (..),
) where

-- Imports.
-- Base.
import Data.Void (Void)

-- Non-hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.ParserT (ParserT, lookAhead)


{- | The t'InputError' error type. -}
newtype InputError = InputError Word
    deriving stock (Eq, Show)

{- | Monofunctoriality of t'InputError'. -}
instance MonoFunctor Word InputError where
    {-# INLINE monomap #-}
    monomap :: (Word -> Word) -> InputError -> InputError
    monomap f (InputError n) = InputError (f n)


{- | The @Streamable@ typeclass of input streams. -}
class (Monad m, MonoFunctor a s) => Streamable m a s | s -> a where
    {-# MINIMAL one #-}

    {- | Get the first element from the stream. -}
    one :: ParserT s InputError m a

    {- | Monadic check for nullity of the input stream. -}
    {-# INLINE eoi #-}
    eoi :: ParserT s Void m Bool
    eoi = fmap (either (const True) (const False)) (lookAhead one)
