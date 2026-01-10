{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * Types.
    ParseError (..),
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)


{- | The @ParseError@ type with error tag @e@ and input stream @s@. -}
type ParseError :: Type -> Type -> Type
data ParseError s e
    = Failure
    | ParseError !s e
    deriving stock (Eq, Show, Functor)


{- | Functoriality in the input strem type @s@. -}
instance Bifunctor ParseError where
    {-# INLINE bimap #-}
    bimap :: (s -> t) -> (d -> e) -> ParseError s d -> ParseError t e
    bimap _ _ Failure           = Failure
    bimap g f (ParseError xs e) = ParseError (g xs) (f e)

{- | The 'Semigroup' instance of t'ParseError'.

The 'Semigroup' instance implements short-circuiting by returning the left, or first, non-identity
element. The instance is idempotent and for every @f :: d -> e@ ane every @g :: s -> t@,

@
'bimap' g f :: t'ParseError' s d -> t'ParseError' t e
@

is a monoid morphism.
-}
instance Semigroup (ParseError s e) where
    {-# INLINE (<>) #-}
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Failure e = e
    (<>) e       _ = e

instance Monoid (ParseError s e) where
    {-# INLINE mempty #-}
    mempty :: ParseError s e
    mempty = Failure
