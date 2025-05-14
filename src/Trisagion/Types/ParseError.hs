{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The 'ParseError' error type.
    ParseError,

    -- ** Prisms.
    empty,
    singleton,

    -- ** Constructors.
    cons,

    -- ** Elimination functions.
    backtrace,
    toListWith,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)

-- Libraries.
import Optics (Prism', prism', review, preview)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

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
    Singleton :: !(ErrorItem s e) -> ParseError s e

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
    monomap f (Singleton err) = Singleton (first f err)
    monomap f (Cons err back) = Cons (first f err) back

instance (Eq s, Eq e) => Eq (ParseError s e) where
    {-# INLINEABLE (==) #-}
    (==) :: ParseError s e -> ParseError s e -> Bool
    (==) Empty Empty = True
    (==) (Singleton e) (Singleton e') = e == e'
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


{- | Prism for the empty value.

The empty value can also be constructed by 'mempty'.
-}
{-# INLINE empty #-}
empty :: Prism' (ParseError s e) ()
empty = prism' construct match
    where
        construct :: () -> ParseError s e
        construct _ = Empty

        match :: ParseError s e -> Maybe ()
        match Empty = Just ()
        match _     = Nothing

{- | Prism for 'ParseError' values without a backtrace. -}
{-# INLINE singleton #-}
singleton :: Prism' (ParseError s e) (s, e)
singleton = prism' construct match
    where
        construct :: (s, e) -> ParseError s e
        construct = Singleton . review errorItem 

        match :: ParseError s e -> Maybe (s, e)
        match (Singleton err) = preview errorItem err
        match _               = Nothing


{- | Construct a 'ParseError' from an error and a 'ParseError'.

note(s):

  * Performs normalization on an empty backtrace.
-}
{-# INLINE cons #-}
cons :: (Typeable d, Eq d, Show d) => s -> e -> ParseError s d -> ParseError s e
cons xs e Empty = curry (review singleton) xs e
cons xs e trace = Cons (curry (review errorItem) xs e) trace


{- | Getter for the top t'ErrorItem' and backtrace. -}
{-# INLINE backtrace #-}
backtrace :: (forall d . ErrorItem s d -> a) -> ParseError s e -> Maybe (ErrorItem s e, [a])
backtrace f (Cons err trace) = Just (err, toListWith f trace)
backtrace _ _                = Nothing

{- | Convert a 'ParseError' to a list. -}
{-# INLINEABLE toListWith #-}
toListWith :: forall s e a . (forall d . ErrorItem s d -> a) -> ParseError s e -> [a]
toListWith f = go
    where
        go :: ParseError s c -> [a]
        go Empty            = []
        go (Singleton err)  = [f err]
        go (Cons err trace) = f err : go trace
