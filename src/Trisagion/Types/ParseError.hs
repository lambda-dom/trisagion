{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The basic error type.
    ErrorItem,

    -- ** Constructors.
    makeErrorItem,

    -- ** Lenses.
    input,
    tag,

    -- * The 'ParseError' error type.
    ParseError,

    -- ** Constructors.
    singleton,
    cons,

    -- ** Elimination functions.
    trace,
    top,
    unconsWith,
) where

-- Imports.
-- Base.
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)

-- Library.
import Optics.Lens (Lens', lens)


{- | The t'ErrorItem' error type with input stream @s@ and error tag @e@. -}
data ErrorItem s e = ErrorItem !s !e
    deriving stock (Eq, Show, Functor)


{- | Construct an t'ErrorItem' from an input stream and an error tag. -}
{-# INLINE makeErrorItem #-}
makeErrorItem :: s -> e -> ErrorItem s e
makeErrorItem = ErrorItem


{- | The lens for the input stream of an t'ErrorItem'. -}
{-# INLINE input #-}
input :: Lens' (ErrorItem s e) s
input = lens get set
    where
        get :: ErrorItem s e -> s
        get (ErrorItem xs _) = xs

        set :: ErrorItem s e -> s -> ErrorItem s e
        set (ErrorItem _ err) xs = ErrorItem xs err

{- | The lens for the error tag of an t'ErrorItem'. -}
{-# INLINE tag #-}
tag :: Lens' (ErrorItem s e) e
tag = lens get set
    where
        get :: ErrorItem s e -> e
        get (ErrorItem _ err) = err

        set :: ErrorItem s e -> e -> ErrorItem s e
        set (ErrorItem xs _) = ErrorItem xs


{- | The 'ParseError' type. -}
data ParseError s e where
    -- | The Empty constructor, the monoid unit for 'ParseError'.
    Empty :: ParseError s e

    -- | A 'ParseError' with no backtrace.
    Single :: !(ErrorItem s e) -> ParseError s e

    -- | A 'ParseError' with a backtrace.
    Cons
        :: (Typeable d, Eq d, Show d)
        => !(ErrorItem s e)             -- ^ Error at the top of the stack.
        -> ParseError s d               -- ^ Backtrace.
        -> ParseError s e

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
singleton :: ErrorItem s e -> ParseError s e
singleton = Single

{- | Construct a 'ParseError' from an error and a backtrace.

note(s):

  * Performs normalization on an empty backtrace.
-}
{-# INLINE cons #-}
cons :: (Typeable d, Eq d, Show d) => ErrorItem s e -> ParseError s d -> ParseError s e
cons err Empty = Single err
cons err back  = Cons err back


{- | Getter for the entire trace of a 'ParseError' as an elimination function. -}
{-# INLINEABLE trace #-}
trace :: forall s e a . (forall d . ErrorItem s d -> a) -> ParseError s e -> [a]
trace f = go
    where
        go :: ParseError s c -> [a]
        go Empty           = []
        go (Single err)    = [f err]
        go (Cons err back) = f err : go back

{- | Getter for the t'ErrorItem' at the top of a 'ParseError'. -}
{-# INLINE top #-}
top :: ParseError s e -> Maybe (ErrorItem s e)
top Empty        = Nothing
top (Single err) = Just err
top (Cons err _) = Just err

{- | Uncons a 'ParseError' with an elimination function. -}
{-# INLINE unconsWith #-}
unconsWith :: (forall d . ErrorItem s d -> a) -> ParseError s e -> Maybe (ErrorItem s e, [a])
unconsWith _ Empty           = Nothing
unconsWith _ (Single err)    = Just (err, [])
unconsWith f (Cons err back) = Just (err, trace f back)
