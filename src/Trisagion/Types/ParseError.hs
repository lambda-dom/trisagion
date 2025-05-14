{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The 'ParseError' error type.
    ParseError,

    -- ** Constructors.
    singleton,
    backtrace,

    -- ** Elimination functions.
    toListWith,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Libraries.
import Optics (review)

-- Package.
import Trisagion.Types.ErrorItem (ErrorItem, errorItem)


{- | The 'ParseError' type.

Functionally, a @'ParseError' s e@ is a, possibly empty, list of t'ErrorItem' values, with a
@t'ErrorItem' s e@ value in the head and a tail of @t'ErrorItem' s d@ values with @d@ an unknown
type with constraints @('Typeable' d, 'Eq' d, 'Show' d)@.
-}
data ParseError s e where
    -- | The Empty constructor, the monoid unit for 'ParseError'.
    Empty :: ParseError s e

    -- | A 'ParseError' with no backtrace.
    Single :: !(ErrorItem s e) -> ParseError s e

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
    monomap _ Empty           = Empty
    monomap f (Single err)    = Single (first f err)
    monomap f (Cons err back) = Cons (first f err) back

instance (Eq s, Eq e) => Eq (ParseError s e) where
    {-# INLINEABLE (==) #-}
    (==) :: ParseError s e -> ParseError s e -> Bool
    (==) Empty Empty = True
    (==) (Single e) (Single e') = e == e'
    (==) (Cons e (b :: ParseError s d) ) (Cons e' (b' :: ParseError s d')) =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e' && b == b'
    (==) _ _ = False

instance Semigroup (ParseError s e) where
    {-# INLINE (<>) #-}
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Empty x = x
    (<>) x    _ = x

instance Monoid (ParseError s e) where
    {-# INLINE mempty #-}
    mempty :: ParseError s e
    mempty = Empty


{- | Construct a 'ParseError' with no backtrace -}
{-# INLINE singleton #-}
singleton :: s -> e -> ParseError s e
singleton xs = Single . review errorItem . (xs, )

{- | Construct a 'ParseError' from an error and a backtrace.

note(s):

  * Performs normalization on an empty backtrace.
-}
{-# INLINE backtrace #-}
backtrace :: (Typeable d, Eq d, Show d) => s -> e -> ParseError s d -> ParseError s e
backtrace xs e Empty = Single (curry (review errorItem) xs e)
backtrace xs e back  = Cons (curry (review errorItem) xs e) back


{- | Convert a 'ParseError' to a list. -}
{-# INLINEABLE toListWith #-}
toListWith :: forall s e a . (forall d . ErrorItem s d -> a) -> ParseError s e -> [a]
toListWith f = go
    where
        go :: ParseError s c -> [a]
        go Empty           = []
        go (Single err)    = [f err]
        go (Cons err back) = f err : go back
