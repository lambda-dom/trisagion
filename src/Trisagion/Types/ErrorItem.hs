{- |
Module: Trisagion.Types.ErrorItem

The @ErrorItem@ error type.
-}

module Trisagion.Types.ErrorItem (
    -- * Error type.
    ErrorItem,

    -- ** Prisms.
    endOfInput,
    errorItem,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Library.
import Optics (Prism', prism')


{- | The t'ErrorItem' error type. -}
data ErrorItem s e
    = EndOfInput !Word                  -- ^ End of input error. Argument is the amount requested.
    | ErrorItem !s !e                   -- ^ Error with input stream @s@ and error tag @e@.
    deriving stock (Eq, Show, Functor)

-- Instances.
instance Bifunctor ErrorItem where
    {-# INLINE bimap #-}
    bimap :: (s -> t) -> (d -> e) -> ErrorItem s d -> ErrorItem t e
    bimap _ _ (EndOfInput n) = EndOfInput n
    bimap f g (ErrorItem xs err) = ErrorItem (f xs) (g err)


{- | Prism for the end of input error.

@'Word'@ parameter is the amount of elements requested, @0@ if this cannot be determined.
-}
{-# INLINE endOfInput #-}
endOfInput :: Prism' (ErrorItem s e) Word
endOfInput = prism' construct match
    where
        construct :: Word -> ErrorItem s e
        construct = EndOfInput

        match :: ErrorItem s e -> Maybe Word
        match (EndOfInput n) = Just n
        match _              = Nothing

{- | Prism for an t'ErrorItem' with input stream @s@ and error tag @e@. -}
{-# INLINE errorItem #-}
errorItem :: Prism' (ErrorItem s e) (s, e)
errorItem = prism' construct match
    where
        construct :: (s, e) -> ErrorItem s e
        construct = uncurry ErrorItem

        match :: ErrorItem s e -> Maybe (s, e)
        match (ErrorItem xs e) = Just (xs, e)
        match _                = Nothing
