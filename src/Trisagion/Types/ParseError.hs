{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * Error tag types.
    ValidationError,

    -- ** Isomorphisms.
    validationError,

    -- * The @ParseError@ type.
    ParseError,

    -- ** Prisms.
    nil,
    singleton,
    cons,

    -- ** Constructors helpers.
    makeEOI,
    makeParseError,
    makeBacktrace,
) where

-- Imports.
-- Base.
import Data.Functor.Identity (Identity (..))
import Data.Typeable (Typeable)

-- Libraries.
import Optics.Core (Prism', Iso', (%), prism', review, iso)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Types.ErrorItem (ErrorItem, TraceItem, endOfInput, errorItem, traceItem)


{- | The t'ValidationError' error tag type thrown on failed validations. -}
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)
    deriving (Applicative, Monad) via Identity


{- | The isomorphism @ValidationError e -> e@. -}
{-# INLINE validationError #-}
validationError :: Iso' (ValidationError e) e
validationError = iso to from
    where
        to :: ValidationError e -> e
        to (ValidationError e) = e

        from :: e -> ValidationError e
        from = ValidationError


{- | The 'ParseError' type.

Functionally, a @'ParseError' e@ is a @'ErrorItem' e@, the top of the entire error trace, and a
list of @forall d . 'ErrorItem' d@ wrapped in a @'TraceItem'@.
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

{- | The Semigroup instance of t'ParseError'.

The 'Semigroup' instance implements short-circuiting by returning the left, or first, non-identity
element. The instance is idempotent and for every @f :: d -> e@,
@'fmap' f :: t'ParseError' d -> t'ParseError' e@ is a monoid morphism.
-}
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

{- | Constructor helper to build a 'ParseError' with no backtrace. -}
{-# INLINE makeParseError #-}
makeParseError
    :: HasOffset s
    => s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError e
makeParseError xs e = review (singleton % errorItem) (offset xs, e)

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
