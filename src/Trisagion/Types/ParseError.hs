{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * Types.
    ParseError,

    -- ** Constructors.
    makeParseError,
    makeParseErrorNoBacktrace,

    -- ** Prisms.
    getState,
    getTag,
    getBacktrace,

    -- * Elimination functions.
    withParseError,
    initial,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Typeable, type (:~:) (..), eqT)
import Data.Void (Void)


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
    {-# INLINE fmap #-}
    fmap :: (d -> e) -> ParseError s d -> ParseError s e
    fmap _ Fail                 = Fail
    fmap f (ParseError b s err) = ParseError b s (f err)

{- | The 'Bifunctor' instance.

A typical use case is, for streams with a notion of position (see 'Trisagion.Typeclasses.HasPosition.HasPosition'),
to capture the position of the stream instead of the stream itself:

@
capture :: 'ParseError' s e -> 'ParseError' (PositionOf s) e
capture = first getPosition
@
-}
instance Bifunctor ParseError where
    {-# INLINE bimap #-}
    bimap :: forall s t d e . (s -> t) -> (d -> e) -> ParseError s d -> ParseError t e
    bimap _ _ Fail               = Fail
    bimap f g (ParseError b s e) = ParseError (go <$> b) (f s) (g e)
        where
            go :: ParseError s c -> ParseError t c
            go Fail                      = Fail
            go (ParseError bt state err) = ParseError (go <$> bt) (f state) err

instance Semigroup (ParseError s e) where
    {-# INLINE (<>) #-}
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Fail x = x
    (<>) x    _ = x

instance Monoid (ParseError s e) where
    {-# INLINE mempty #-}
    mempty :: ParseError s e
    mempty = Fail


{- | Constructor helper to create t'ParseError' values. -}
{-# INLINE makeParseError #-}
makeParseError
    :: (Typeable d, Eq d, Show d)
    => ParseError s d               -- ^ Backtrace.
    -> s                            -- ^ State component.
    -> e                            -- ^ Error tag.
    -> ParseError s e
makeParseError b = ParseError (Just b)

{- | Constructor helper to create a t'ParseError' capturing the stream and with no backtrace.

note(s):

    * The backtrace is a @'Nothing'@ of type @'Maybe' (t'ParseError' s 'Void')@.
-}
{-# INLINE makeParseErrorNoBacktrace #-}
makeParseErrorNoBacktrace
    :: s                            -- ^ State component.
    -> e                            -- ^ Error tag.
    -> ParseError s e
makeParseErrorNoBacktrace =
    let b = Nothing :: Maybe (ParseError s Void) in
        ParseError b


{- | Getter for the state component. -}
{-# INLINE getState #-}
getState :: ParseError s e -> Maybe s
getState Fail               = Nothing
getState (ParseError _ s _) = Just s

{- | Getter for the error tag. -}
{-# INLINE getTag #-}
getTag :: ParseError s e -> Maybe e
getTag Fail               = Nothing
getTag (ParseError _ _ e) = Just e

{- | Getter for the backtrace of an error as an elimination function. -}
getBacktrace :: forall s e a . (forall d . s -> d -> a) -> ParseError s e -> [a]
getBacktrace f = go
    where
        go :: ParseError s c -> [a]
        go Fail               = []
        go (ParseError r s e) = f s e : maybe [] go r


{- | Case analysis elimination function for the t'ParseError' type. -}
{-# INLINE withParseError #-}
withParseError :: b -> (forall d . Maybe (ParseError s d) -> s -> e -> b) -> ParseError s e -> b
withParseError x _ Fail               = x
withParseError _ f (ParseError b s e) = f b s e

{- | The universal property of the initial monoid @t'ParseError' s 'Void'@. -}
{-# INLINE initial #-}
initial :: Monoid m => ParseError s Void -> m
initial e =
    case e of
        Fail -> mempty
