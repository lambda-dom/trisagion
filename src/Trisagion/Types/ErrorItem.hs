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
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)

-- Library.
import Optics.Prism (Prism', prism')


{- | The t'ErrorItem' error type. -}
data ErrorItem e
    = EndOfInput {-# UNPACK #-} !Word   -- ^ End of input error. Argument is the amount requested.
    | ErrorItem {-# UNPACK #-} !Word !e -- ^ Error with input stream offset and error tag @e@.
    deriving stock (Eq, Show, Functor)


{- | Prism for the end of input error.

@'Word'@ parameter is the amount of elements requested, @0@ if this cannot be determined.
-}
{-# INLINE endOfInput #-}
endOfInput :: Prism' (ErrorItem e) Word
endOfInput = prism' construct match
    where
        construct :: Word -> ErrorItem e
        construct = EndOfInput

        match :: ErrorItem e -> Maybe Word
        match (EndOfInput n) = Just n
        match _              = Nothing

{- | Prism for an t'ErrorItem' with input stream offset @n@ and error tag @e@. -}
{-# INLINE errorItem #-}
errorItem :: Prism' (ErrorItem e) (Word, e)
errorItem = prism' construct match
    where
        construct :: (Word, e) -> ErrorItem e
        construct = uncurry ErrorItem

        match :: ErrorItem e -> Maybe (Word, e)
        match (ErrorItem n e) = Just (n, e)
        match _               = Nothing


{- | The @TraceItem@ type, a wrapper around @forall d . 'ErrorItem' d@. -}
data TraceItem where
    TraceItem :: (Typeable d, Eq d, Show d) => !(ErrorItem d) -> TraceItem

-- Instances.
deriving stock instance Show TraceItem

instance Eq TraceItem where
    {-# INLINEABLE (==) #-}
    (==) :: TraceItem -> TraceItem -> Bool
    (==) (TraceItem (e :: ErrorItem d)) (TraceItem (e' :: ErrorItem d')) =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e'


{- | The traceItem prism for 'TraceItem'.

Mainly useful for the constructor, as the matcher requires the knowledge of the type @e@ of the
@'ErrorItem' e@ inside @'TraceItem'@, and a runtime check for the downcast.
-}
{-# INLINE traceItem #-}
traceItem :: forall e . (Typeable e, Eq e, Show e) => Prism' TraceItem (ErrorItem e)
traceItem = prism' construct match
    where
        construct :: ErrorItem e -> TraceItem
        construct = TraceItem

        match :: TraceItem -> Maybe (ErrorItem e)
        match (TraceItem (err :: ErrorItem d)) =
            case eqT @d @e of
                Nothing   -> Nothing
                Just Refl -> Just err


{- | Apply a natural transformation @'ErrorItem' -> 'ErrorItem'@ to a 'TraceItem'. -}
{-# INLINE applyNat #-}
applyNat :: (forall d . ErrorItem d -> ErrorItem d) -> TraceItem -> TraceItem
applyNat f (TraceItem e) = TraceItem (f e)

{- | Apply a natural transformation @'ErrorItem' -> Const a@, or a /cone/, to a 'TraceItem'. -}
{-# INLINE applyCone #-}
applyCone :: (forall d . ErrorItem d -> a) -> TraceItem -> a
applyCone f (TraceItem e) = f e
