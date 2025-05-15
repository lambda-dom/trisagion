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
    makeTrace,
    makeEOI,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)

-- Libraries.
import Optics.Prism (Prism', prism')

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Types.ErrorItem (ErrorItem, TraceItem, errorItem, traceItem, endOfInput)
import Optics.Core (review, (%))


{- | The 'ParseError' type. -}
data ParseError s e where
    -- | The Nil constructor.
    Nil :: ParseError s e

    -- | The Cons constructor.
    Cons :: (Typeable e, Eq e, Show e) => !(ErrorItem s e) -> [TraceItem s] -> ParseError s e

-- Instances.
deriving stock instance (Show s, Show e) => Show (ParseError s e)
deriving stock instance (Eq s, Eq e) => Eq (ParseError s e)

{- | Monofunctoriality for 'ParseError' in the input stream type. -}
instance MonoFunctor (ParseError s e) where
    type ElementOf (ParseError s e) = s

    {-# INLINEABLE monomap #-}
    monomap :: (s -> s) -> ParseError s e -> ParseError s e
    monomap _ Nil         = Nil
    monomap f (Cons e es) = Cons (first f e) (fmap (fmap f) es)

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
singleton :: forall s e . (Typeable e, Eq e, Show e) => Prism' (ParseError s e) (ErrorItem s e)
singleton = prism' construct match
    where
        construct :: ErrorItem s e -> ParseError s e
        construct = flip Cons []

        match :: ParseError s e -> Maybe (ErrorItem s e)
        match (Cons e []) = Just e
        match _           = Nothing

{- | The cons prism for 'ParseError' values with a backtrace. -}
{-# INLINE cons #-}
cons :: forall s e . (Typeable e, Eq e, Show e) => Prism' (ParseError s e) (ErrorItem s e, [TraceItem s])
cons = prism' construct match
    where
        construct :: (ErrorItem s e, [TraceItem s]) -> ParseError s e
        construct = uncurry Cons

        match :: ParseError s e -> Maybe (ErrorItem s e, [TraceItem s])
        match Nil         = Nothing
        match (Cons e es) = Just (e, es)


{- | Constructor helper to build a 'ParseError' with a backtrace. -}
{-# INLINE makeTrace #-}
makeTrace :: (Typeable e, Eq e, Show e) => s -> e -> ParseError s d -> ParseError s e
makeTrace xs e Nil         = review (singleton % errorItem) (xs, e)
makeTrace xs e (Cons y ys) = Cons (review errorItem (xs, e)) (review traceItem y : ys)

{- | Constructor helper to build an end of input 'ParseError'. -}
makeEOI :: (Typeable e, Eq e, Show e) => Word -> ParseError s e
makeEOI = review (singleton % endOfInput)
