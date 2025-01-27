{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * Types.
    ParseError (..),

    -- ** Constructors.
    makeParseError,

    -- ** Prisms.
    getState,
    getTag,
    getBacktrace,

    -- ** Elimination functions.
    withParseError,
    initial,

    -- ** Generalized mapping.
    mapWith,
) where

-- Imports.
-- Base.
import Data.Data (Typeable, (:~:) (Refl), eqT)

-- Libraries.
import Data.Void (Void)

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition (..))


{- | The @ParseError s e@ error type. -}
data ParseError s e where
    -- | Error with no indication of its origin.
    Fail :: ParseError s e
    ParseError
        :: (Show d, Eq d, Typeable d)
        => Maybe (ParseError s d)   -- ^ Error backtrace.
        -> !s                       -- ^ State component.
        -> !e                       -- ^ Error tag.
        -> ParseError s e

-- Instances.
deriving stock instance (Show s, Show e) => Show (ParseError s e)

instance (Eq s, Eq e) => Eq (ParseError s e) where
    (==) :: ParseError s e -> ParseError s e -> Bool
    (==) (ParseError (b :: Maybe (ParseError s d)) s e) (ParseError (b' :: Maybe (ParseError s d')) s' e') =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e' && s == s' && b == b'
    (==) Fail Fail = True
    (==) _    _    = False

instance Functor (ParseError s) where
    fmap :: (d -> e) -> ParseError s d -> ParseError s e
    fmap = mapWith id id

instance Semigroup (ParseError s e) where
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Fail x = x
    (<>) x    _ = x

instance Monoid (ParseError s e) where
    mempty :: ParseError s e
    mempty = Fail


{- | Constructor helper to create a t'ParseError' capturing the position of the stream and with no backtrace. -}
makeParseError
    :: forall s e . (HasPosition s)
    => s
    -> e
    -> ParseError (PositionOf s) e
makeParseError s =
    let b = Nothing :: Maybe (ParseError (PositionOf s) Void) in
        ParseError b (getPosition s)


{- | Getter for the error state component. -}
getState :: ParseError s e -> Maybe s
getState = withParseError Nothing (\ _ s _ -> Just s)

{- | Getter for the error tag. -}
getTag :: ParseError s e -> Maybe e
getTag = withParseError Nothing (\ _ _ e -> Just e)

{- | Getter for the backtrace of an error as an elimination function. -}
getBacktrace :: (forall d . s -> d -> a) -> ParseError s e -> [a]
getBacktrace f  = go
    where
        go (ParseError r s e) = f s e : maybe [] (getBacktrace f) r
        go Fail               = []


{- | Case analysis elimination function for the t'ParseError' type. -}
withParseError
    :: a
    -> (forall d . Maybe (ParseError s d) -> s -> e -> a)
    -> ParseError s e
    -> a
withParseError _ f (ParseError b s e) = f b s e
withParseError x _ Fail               = x

{- | The universal property of the initial monoid @t'ParseError' s 'Void'@. -}
initial :: Monoid e => ParseError s Void -> e
initial e =
    case e of
        Fail -> mempty


{- | Generalized @'fmap'@ to provide monofunctoriality over the state and the backtrace. -}
mapWith
    :: (forall c . ParseError s c -> ParseError s c)    -- ^ Map over the backtrace.
    -> (s -> s)                                         -- ^ Map over the state.
    -> (d -> e)                                         -- ^ Map over the error tag.
    -> ParseError s d
    -> ParseError s e
mapWith h g f (ParseError b s e) = ParseError (h <$> b) (g s) (f e)
mapWith _ _ _ Fail               = Fail
