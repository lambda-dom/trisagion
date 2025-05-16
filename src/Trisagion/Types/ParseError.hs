{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * The @ParseError@ type.
    ParseError,

    -- ** Prisms.
    nil,
    singleton,
    cons,

    -- ** Constructors.
    makeEOI,
    makeBacktrace,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)

-- Libraries.
import Optics.Core (Prism', (%), prism', review)

-- Package.
import Trisagion.Types.ErrorItem (ErrorItem, TraceItem, endOfInput, errorItem, traceItem)


{- | The 'ParseError' type.

Functionally, a @'ParseError' s e@ is a @'ErrorItem' s e@, the top of the entire error trace, and a
list of @forall d . 'ErrorItem' s d@ wrapped in a @'TraceItem' s@.
-}
data ParseError s e where
    -- | The Nil constructor.
    Nil :: ParseError s e

    -- | The Cons constructor.
    Cons :: !(ErrorItem s e) -> [TraceItem s] -> ParseError s e

-- Instances.
deriving stock instance (Eq s, Eq e) => Eq (ParseError s e)
deriving stock instance (Show s, Show e) => Show (ParseError s e)
deriving stock instance Functor (ParseError s)

instance Bifunctor ParseError where
    {-# INLINE bimap #-}
    bimap :: (s -> t) -> (d -> e) -> ParseError s d -> ParseError t e
    bimap _ _ Nil         = Nil
    bimap f g (Cons e es) = Cons (bimap f g e) (fmap (fmap f) es)

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

{- | The singleton prism for 'ParseError' values with no backtrace. -}
{-# INLINE singleton #-}
singleton :: Prism' (ParseError s e) (ErrorItem s e)
singleton = prism' construct match
    where
        construct :: ErrorItem s e -> ParseError s e
        construct e = Cons e []

        match :: ParseError s e -> Maybe (ErrorItem s e)
        match (Cons e []) = Just e
        match _           = Nothing

{- | The cons prism for 'ParseError' values with a backtrace. -}
{-# INLINE cons #-}
cons :: Prism' (ParseError s e) (ErrorItem s e, [TraceItem s])
cons = prism' construct match
    where
        construct :: (ErrorItem s e, [TraceItem s]) -> ParseError s e
        construct = uncurry Cons

        match :: ParseError s e -> Maybe (ErrorItem s e, [TraceItem s])
        match (Cons e es) = Just (e, es)
        match _           = Nothing


{- | Constructor helper to build an end of input 'ParseError'. -}
{-# INLINE makeEOI #-}
makeEOI :: Word -> ParseError s e
makeEOI = review (singleton % endOfInput)

{- | Constructor helper to build a 'ParseError' with a backtrace. -}
{-# INLINE makeBacktrace #-}
makeBacktrace
    :: (Typeable d, Eq d, Show d)
    => s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError s d                   -- ^ Backtrace.
    -> ParseError s e
makeBacktrace xs e Nil         = review (singleton % errorItem) (xs, e)
makeBacktrace xs e (Cons y ys) = Cons (review errorItem (xs, e)) (review traceItem y : ys)
