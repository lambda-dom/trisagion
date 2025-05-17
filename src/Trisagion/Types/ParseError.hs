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
import Data.Typeable (Typeable)

-- Libraries.
import Optics.Core (Prism', (%), prism', review)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Types.ErrorItem (ErrorItem, TraceItem, endOfInput, errorItem, traceItem)


{- | The 'ParseError' type.

Functionally, a @'ParseError' s e@ is a @'ErrorItem' s e@, the top of the entire error trace, and a
list of @forall d . 'ErrorItem' s d@ wrapped in a @'TraceItem' s@.
-}
data ParseError e where
    -- | The Nil constructor.
    Nil :: ParseError e

    -- | The Cons constructor.
    Cons :: !(ErrorItem e) -> [TraceItem] -> ParseError e

-- Instances.
deriving stock instance Eq e => Eq (ParseError e)
deriving stock instance Show e => Show (ParseError e)
deriving stock instance Functor ParseError

instance Semigroup (ParseError e) where
    {-# INLINE (<>) #-}
    (<>) :: ParseError e -> ParseError e -> ParseError e
    (<>) Nil x = x
    (<>) x   _ = x

instance Monoid (ParseError e) where
    {-# INLINE mempty #-}
    mempty :: ParseError e
    mempty = Nil

{- | Prism for the nil value.

The nil value can also be constructed by 'mempty'.
-}
{-# INLINE nil #-}
nil :: Prism' (ParseError e) ()
nil = prism' construct match
    where
        construct :: () -> ParseError e
        construct _ = Nil

        match :: ParseError e -> Maybe ()
        match Nil = Just ()
        match _   = Nothing

{- | The singleton prism for 'ParseError' values with no backtrace. -}
{-# INLINE singleton #-}
singleton :: Prism' (ParseError e) (ErrorItem e)
singleton = prism' construct match
    where
        construct :: ErrorItem e -> ParseError e
        construct e = Cons e []

        match :: ParseError e -> Maybe (ErrorItem e)
        match (Cons e []) = Just e
        match _           = Nothing

{- | The cons prism for 'ParseError' values with a backtrace. -}
{-# INLINE cons #-}
cons :: Prism' (ParseError e) (ErrorItem e, [TraceItem])
cons = prism' construct match
    where
        construct :: (ErrorItem e, [TraceItem]) -> ParseError e
        construct = uncurry Cons

        match :: ParseError e -> Maybe (ErrorItem e, [TraceItem])
        match (Cons e es) = Just (e, es)
        match _           = Nothing


{- | Constructor helper to build an end of input 'ParseError'. -}
{-# INLINE makeEOI #-}
makeEOI :: Word -> ParseError e
makeEOI = review (singleton % endOfInput)

{- | Constructor helper to build a 'ParseError' with a backtrace. -}
{-# INLINE makeBacktrace #-}
makeBacktrace
    :: (HasOffset s, Typeable d, Eq d, Show d)
    => s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError d                     -- ^ Backtrace.
    -> ParseError e
makeBacktrace xs e Nil         = review (singleton % errorItem) (offset xs, e)
makeBacktrace xs e (Cons y ys) = Cons (review errorItem (offset xs, e)) (review traceItem y : ys)
