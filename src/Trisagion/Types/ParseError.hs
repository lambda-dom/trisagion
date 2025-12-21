{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * Types.
    ParseError (..),

    -- * Merging errors.
    cozip,
) where

-- Imports.
-- Base.
import Data.Kind (Type)

-- Package.
import Trisagion.Types.Result ((:+:))


{- | The t'ParseError' type. -}
type ParseError :: Type -> Type
data ParseError e
    = Failure
    | ParseError !Word e
    deriving stock (Eq, Show, Functor)


{- | The 'Semigroup' instance of t'ParseError'.

The 'Semigroup' instance implements short-circuiting by returning the left, or first, non-identity
element. The instance is idempotent and for every @f :: d -> e@,

@
'fmap' f :: t'ParseError' d -> t'ParseError' e
@

is a monoid morphism.
-}
instance Semigroup (ParseError e) where
    {-# INLINE (<>) #-}
    (<>) :: ParseError e -> ParseError e -> ParseError e
    (<>) Failure e = e
    (<>) e       _ = e

instance Monoid (ParseError e) where
    {-# INLINE mempty #-}
    mempty :: ParseError e
    mempty = Failure


{- | Dual of 'zip' for 'Either'. -}
{-# INLINE cozip #-}
cozip :: Functor f => f a :+: f b -> f (a :+: b)
cozip = either (fmap Left) (fmap Right)
