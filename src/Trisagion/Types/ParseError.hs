{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The 'ParseError' error type.
    ParseError,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Types.ErrorItem (ErrorItem)


{- | The 'ParseError' type.

Functionally, a @'ParseError' s e@ is a, possibly empty, list of t'ErrorItem' values, with a
@t'ErrorItem' s e@ value in the head and a tail of @t'ErrorItem' s d@ values with @d@ an unknown
type with constraints @('Typeable' d, 'Eq' d, 'Show' d)@.
-}
data ParseError s e where
    -- | The Nil constructor, the monoid unit for 'ParseError'.
    Nil :: ParseError s e

    -- | A 'ParseError' with a backtrace.
    Cons :: (Typeable d, Eq d, Show d) => !(ErrorItem s e) -> ParseError s d -> ParseError s e

-- Instances.
deriving stock instance Functor (ParseError s)
deriving stock instance (Show s, Show e) => Show (ParseError s e)

{- | Provides monofunctoriality in the input stream @s@ for @'ParseError' s e@. -}
instance MonoFunctor (ParseError s e) where
    type ElementOf (ParseError s e) = s

    {-# INLINE monomap #-}
    monomap :: (s -> s) -> ParseError s e -> ParseError s e
    monomap _ Nil             = Nil
    monomap f (Cons err back) = Cons (first f err) back

instance (Eq s, Eq e) => Eq (ParseError s e) where
    {-# INLINEABLE (==) #-}
    (==) :: ParseError s e -> ParseError s e -> Bool
    (==) Nil Nil = True
    (==) (Cons e (b :: ParseError s d) ) (Cons e' (b' :: ParseError s d')) =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e' && b == b'
    (==) _ _ = False

instance Semigroup (ParseError s e) where
    {-# INLINE (<>) #-}
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Nil x = x
    (<>) x   _ = x

instance Monoid (ParseError s e) where
    {-# INLINE mempty #-}
    mempty :: ParseError s e
    mempty = Nil

