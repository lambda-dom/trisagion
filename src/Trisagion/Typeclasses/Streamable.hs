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
import Mono.Typeclasses.MonoFunctor (MonoFunctor)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.ParserT (ParserT, lookAhead)


{- | The t'InputError' error type. -}
newtype InputError = InputError Word
    deriving stock (Eq, Show)


{- | The @Streamable@ typeclass of input streams. -}
class (Monad m, MonoFunctor a s) => Streamable m a s | s -> a where
    {-# MINIMAL one #-}

    {- | Get the first element from the stream. -}
    one :: ParserT s (ParseError InputError) m a

    {- | Monadic check for nullity of the input stream. -}
    {-# INLINE eoi #-}
    eoi :: ParserT s Void m Bool
    eoi = fmap (either (const True) (const False)) (lookAhead one)
