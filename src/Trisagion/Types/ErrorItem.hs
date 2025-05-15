{- |
Module: Trisagion.Types.ErrorItem

The @ErrorItem@ type.
-}

module Trisagion.Types.ErrorItem (
    -- * The @ErrorItem@ type.
    ErrorItem (..),

    -- ** Prisms.
    endOfInput,
    errorItem,

    -- * The @TraceItem@ type.
    TraceItem,

    -- ** Prisms.
    traceItem,

    -- * Functions.
    applyNat,
    applyCone,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)

-- Library.
import Optics.Prism (Prism', prism')


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


{- | The @TraceItem s@ type, a wrapper around @forall d . 'ErrorItem' s d@. -}
data TraceItem s where
    TraceItem :: (Typeable d, Eq d, Show d) => !(ErrorItem s d) -> TraceItem s

-- Instances.
deriving stock instance (Show s) => Show (TraceItem s)

instance Eq s => Eq (TraceItem s) where
    {-# INLINEABLE (==) #-}
    (==) :: TraceItem s -> TraceItem s -> Bool
    (==) (TraceItem (e :: ErrorItem s d)) (TraceItem (e' :: ErrorItem s d')) =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e'

instance Functor TraceItem where
    {-# INLINE fmap #-}
    fmap :: (s -> t) -> TraceItem s -> TraceItem t
    fmap f = applyNat (first f)


{- | The traceItem prism for 'TraceItem'. -}
{-# INLINE traceItem #-}
traceItem :: forall s e . (Typeable e, Eq e, Show e) => Prism' (TraceItem s) (ErrorItem s e)
traceItem = prism' construct match
    where
        construct :: ErrorItem s e -> TraceItem s
        construct = TraceItem

        match :: TraceItem s -> Maybe (ErrorItem s e)
        match (TraceItem (err :: ErrorItem s d)) =
            case eqT @d @e of
                Nothing   -> Nothing
                Just Refl -> Just err


{- | Apply a natural transformation @'ErrorItem' s -> 'ErrorItem' t@ to a 'TraceItem'. -}
{-# INLINE applyNat #-}
applyNat :: (forall d . ErrorItem s d -> ErrorItem t d) -> TraceItem s -> TraceItem t
applyNat f (TraceItem e) = TraceItem (f e)

{- | Apply a natural transformation @'ErrorItem' s -> Const a@, or a /cone/, to a 'TraceItem'. -}
{-# INLINE applyCone #-}
applyCone :: (forall d . ErrorItem s d -> a) -> TraceItem s -> a
applyCone f (TraceItem e) = f e
