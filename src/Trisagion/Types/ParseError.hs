{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * Types.
    ParseError (..),

    -- ** Constructors helpers.
    makeParseError,

    -- * Merging errors.
    cozip,
) where

-- Imports.
-- Package.
import Trisagion.Types.Result ((:+:))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))


{- | The t'ParseError' type. -}
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


{- | Constructor helper for t'ParseError'. -}
makeParseError
    :: (HasOffset m a s, Monad m)
    => s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> m (ParseError e)
makeParseError xs e = do
    n <- offset xs
    pure (ParseError n e)

{- | Dual of 'zip' for 'Either'. -}
cozip :: Functor f => f a :+: f b -> f (a :+: b)
cozip = either (fmap Left) (fmap Right)
