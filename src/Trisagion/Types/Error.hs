{- |
Module: Trisagion.Types.Error

The @Error@ error type.
-}

module Trisagion.Types.Error (
    -- * Error type.
    Error,

    -- ** Constructors.
    makeError,

    -- ** Lenses.
    input,
    tag,

) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Library.
import Optics.Lens (Lens', lens)


{- | The t'Error' error type with input stream @s@ and error tag @e@. -}
data Error s e = Error !s !e
    deriving stock (Eq, Show, Functor)

-- Instances.
instance Bifunctor Error where
    {-# INLINE bimap #-}
    bimap :: (s -> t) -> (d -> e) -> Error s d -> Error t e
    bimap f g (Error xs err) = Error (f xs) (g err)


{- | Construct an t'Error' from an input stream and an error tag. -}
{-# INLINE makeError #-}
makeError :: s -> e -> Error s e
makeError = Error


{- | The lens for the input stream of an t'Error'. -}
{-# INLINE input #-}
input :: Lens' (Error s e) s
input = lens get set
    where
        get :: Error s e -> s
        get (Error xs _) = xs

        set :: Error s e -> s -> Error s e
        set (Error _ err) xs = Error xs err

{- | The lens for the error tag of an t'Error'. -}
{-# INLINE tag #-}
tag :: Lens' (Error s e) e
tag = lens get set
    where
        get :: Error s e -> e
        get (Error _ err) = err

        set :: Error s e -> e -> Error s e
        set (Error xs _) = Error xs
