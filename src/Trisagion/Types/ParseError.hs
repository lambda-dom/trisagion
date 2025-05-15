{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The 'ParseError' error type.
    ParseError,

    -- ** Prisms.
    nil,

    -- * The 'Backtrace' type.
    Backtrace,

    -- ** Prisms.
    backtrace,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)

-- Libraries.
-- import Optics.Optic ((%))
import Optics.Prism (Prism', prism')
-- import Optics.Review (review)

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

{- | Provides functoriality in the input stream @s@ for @'ParseError' s e@.

Applies the function @f@ to all 'ErrorItem' in the 'ParseError'. If need to apply the function
only to the top error use the @cons@ prism.
-}
instance MonoFunctor (ParseError s e) where
    type ElementOf (ParseError s e) = s

    {-# INLINEABLE monomap #-}
    monomap :: (s -> s) -> ParseError s e -> ParseError s e
    monomap _ Nil             = Nil
    monomap f (Cons err back) = Cons (first f err) (monomap f back)

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


{- | Prism for the nil value.

The nil value can also be constructed by 'mempty'.
-}
{-# INLINE nil #-}
nil :: Prism' (ParseError s e) ()
nil = prism' construct match
    where
        construct :: () -> ParseError s e
        construct _ = Nil

        match :: ParseError s e -> Maybe ()
        match Nil = Just ()
        match _   = Nothing

{- | The @Backtrace s@ type, a wrapper around @forall d . 'ParseError' s d@. -}
data Backtrace s where
    Backtrace :: (Typeable d, Eq d, Show d) => !(ParseError s d) -> Backtrace s

-- Instances.
deriving stock instance (Show s) => Show (Backtrace s)

instance Eq s => Eq (Backtrace s) where
    {-# INLINEABLE (==) #-}
    (==) :: Backtrace s -> Backtrace s -> Bool
    (==) (Backtrace (e :: ParseError s d)) (Backtrace (e' :: ParseError s d')) =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e'

instance MonoFunctor (Backtrace s) where
    type ElementOf (Backtrace s) = s

    {-# INLINE monomap #-}
    monomap :: (s -> s) -> Backtrace s -> Backtrace s
    monomap f (Backtrace err) = Backtrace (monomap f err)

{- | The backtrace prism for 'Backtrace'. -}
{-# INLINE backtrace #-}
backtrace :: forall s e . (Typeable e, Eq e, Show e) => Prism' (Backtrace s) (ParseError s e)
backtrace = prism' construct match
    where
        construct :: ParseError s e -> Backtrace s
        construct = Backtrace

        match :: Backtrace s -> Maybe (ParseError s e)
        match (Backtrace (err :: ParseError s d)) =
            case eqT @d @e of
                Nothing   -> Nothing
                Just Refl -> Just err
