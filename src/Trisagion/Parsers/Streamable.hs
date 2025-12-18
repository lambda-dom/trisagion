{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Streamable' m a s@ constraints.
-}

module Trisagion.Parsers.Streamable (
    -- * Error types.
    InputError (..),

    -- * Parsers @'Streamable' m a s => 'ParserT' m s e a@.
    eoi,
    one,
) where

-- Imports.
-- Base.
import Data.Void (Void)

-- Package.
import Trisagion.Types.Result (Result (..))
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.ParserT (ParserT, embed)


{- | The t'InputError' error type. -}
newtype InputError = InputError Word
    deriving stock (Eq, Show)


{- | Monadic check for nullity of the input stream. -}
{-# INLINE eoi #-}
eoi :: Streamable m a s => ParserT m s Void Bool
eoi = embed $ \ xs -> do
    r <- nullM xs
    pure (Success r xs)

{- | Parse one element from the input stream. -}
{-# INLINE one #-}
one :: (Streamable m a s, HasOffset m s) => ParserT m s (ParseError InputError) a
one = embed $ \ xs -> do
    n <- offset xs
    r <- unconsM xs
    case r of
        Nothing      -> pure $ Error (ParseError n (InputError 1))
        Just (x, ys) -> pure $ Success x ys
