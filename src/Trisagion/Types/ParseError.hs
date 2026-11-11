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
import Data.Kind (Type)


{- | The @ParseError e@ type with error tag @e@.

The @t'ParseError'@ type performs two important functions in dealing with parser errors: (1) it
provides a short-circuiting strategy for accumulating errors by embedding any type in a monoid
(e. g. the function @e -> t'ParseError' e@ is injective) and (2) allows annotating errors with the
input stream position.
-}
type ParseError :: Type -> Type
data ParseError e
    = Failure
    | ParseError !Int e
    deriving stock (Eq, Show, Functor)


{- | The 'Semigroup' instance of t'ParseError'.

The 'Semigroup' instance implements short-circuiting by returning the left, or first, non-identity
element. The instance is idempotent and for every @f :: d -> e@,

@
'fmap' f :: t'ParseError' d -> t'ParseError' e
@

is a monoid morphism. With this monoid structure, @t'ParseError' e@ is monoid-isomorphic to
@'Maybe' (t'Data.First.First' e)@.
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
