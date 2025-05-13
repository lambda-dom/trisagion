{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The 'ParseError' error type.
    ParseError,

    -- ** Constructors.
    singleton,
    cons,

    -- ** Elimination functions.
    trace,
    top,

    -- ** Functions.
    modify,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)

-- Package.
import Trisagion.Types.Error (Error, makeError)


{- | The 'ParseError' type.

Functionally, a @'ParseError' s e@ is a, possibly empty, list of t'Error' values, with a
@t'Error' s e@ value in the head and a tail of @t'Error' s d@ values with @d@ an unknown type with
constraints @('Typeable' d, 'Eq' d, 'Show' d)@.
-}
data ParseError s e where
    -- | The Empty constructor, the monoid unit for 'ParseError'.
    Empty :: ParseError s e

    -- | A 'ParseError' with no backtrace.
    Single :: !(Error s e) -> ParseError s e

    -- | A 'ParseError' with a backtrace.
    Cons :: (Typeable d, Eq d, Show d) => !(Error s e) -> ParseError s d -> ParseError s e

-- Instances.
deriving stock instance Functor (ParseError s)
deriving stock instance (Show s, Show e) => Show (ParseError s e)

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
singleton xs = Single . makeError xs

{- | Construct a 'ParseError' from an error and a backtrace.

note(s):

  * Performs normalization on an empty backtrace.
-}
{-# INLINE cons #-}
cons :: (Typeable d, Eq d, Show d) => s -> e -> ParseError s d -> ParseError s e
cons xs e Empty = Single (makeError xs e)
cons xs e back  = Cons (makeError xs e) back


{- | Getter for the entire trace of a 'ParseError' as an elimination function. -}
{-# INLINEABLE trace #-}
trace :: forall s e a . (forall d . Error s d -> a) -> ParseError s e -> [a]
trace f = go
    where
        go :: ParseError s c -> [a]
        go Empty           = []
        go (Single err)    = [f err]
        go (Cons err back) = f err : go back

{- | Getter for the t'Error' at the top of a 'ParseError'. -}
{-# INLINE top #-}
top :: ParseError s e -> Maybe (Error s e)
top Empty        = Nothing
top (Single err) = Just err
top (Cons err _) = Just err


{- | Apply a function to the input stream of the top error in a 'ParseError'.

Provides monofunctoriality for 'ParseError' in the input stream type @s@.
-}
{-# INLINE modify #-}
modify :: (s -> s) -> ParseError s e -> ParseError s e
modify _ Empty           = Empty
modify f (Single err)    = Single (first f err)
modify f (Cons err back) = Cons (first f err) back
