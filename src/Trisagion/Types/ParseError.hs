{- |
Module: Trisagion.Types.ParseError

The @ParseError@ error type.
-}

module Trisagion.Types.ParseError (
    -- * Types.
    ParseError,

    -- ** Constructors.
    makeParseError,
    makeBacktrace,

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
import Data.Typeable (Typeable, type (:~:) (..), eqT)
import Data.Void (Void)


{- | The @ParseError s e@ error type. -}
data ParseError s e where
    -- | Error with no indication of its origin.
    Fail :: ParseError s e

    -- | Error without a backtrace.
    Error :: !s -> !e -> ParseError s e

    -- | Error with a backtrace.
    Backtrace
        :: (Typeable d, Eq d, Show d)
        => !(ParseError s d)              -- ^ Backtrace.
        -> !s                             -- ^ Input stream.
        -> !e                             -- ^ Error tag.
        -> ParseError s e


-- Instances.
deriving stock instance (Show s, Show e) => Show (ParseError s e)
deriving stock instance Functor (ParseError s)

{- | The 'Bifunctor' instance.

A typical use case is, for input streams with a notion of position, to capture the position of the
stream instead of the stream itself -- see 'Trisagion.Typeclasses.HasPosition.HasPosition':

@
capture :: 'ParseError' s e -> 'ParseError' (PositionOf s) e
capture = first getPosition
@
-}
instance Bifunctor ParseError where
    bimap :: forall s t d e . (s -> t) -> (d -> e) -> ParseError s d -> ParseError t e
    bimap _ _ Fail              = Fail
    bimap f g (Error s e)       = Error (f s) (g e)
    bimap f g (Backtrace b s e) = Backtrace (first f b) (f s) (g e)

instance (Eq s, Eq e) => Eq (ParseError s e) where
    (==) :: ParseError s e -> ParseError s e -> Bool
    (==) Fail Fail = True
    (==) (Error s e) (Error s' e') = e == e' && s == s'
    (==) (Backtrace (b :: ParseError s d) s e ) (Backtrace (b' :: ParseError s d') s' e') =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e' && s == s' && b == b'
    (==) _    _    = False

instance Semigroup (ParseError s e) where
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Fail x = x
    (<>) x    _ = x

instance Monoid (ParseError s e) where
    mempty :: ParseError s e
    mempty = Fail


{- | Construct a 'ParseError' value with a backtrace.

note(s):

    * Since a @Fail@ backtrace is functionally equivalent to no backtrace, 'makeBacktrace'
    normalizes it.
-}
makeBacktrace
    :: forall d s e . (Typeable d, Eq d, Show d)
    => ParseError s d                   -- ^ Backtrace.
    -> s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError s e
makeBacktrace b =
    case b of
        Fail -> Error
        _    -> Backtrace b

{- | Construct a 'ParseError' value with no backtrace. -}
makeParseError
    :: s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError s e
makeParseError = Error


{- | Prism for the state component. -}
getState :: ParseError s e -> Maybe s
getState Fail              = Nothing
getState (Error s _)       = Just s
getState (Backtrace _ s _) = Just s

{- | Prism for the error tag. -}
getTag :: ParseError s e -> Maybe e
getTag Fail              = Nothing
getTag (Error _ e)       = Just e
getTag (Backtrace _ _ e) = Just e

{- | Getter for the backtrace of an error as an elimination function. -}
getBacktrace :: forall s e a . (forall d . s -> d -> a) -> ParseError s e -> [a]
getBacktrace f = go
    where
        go :: ParseError s c -> [a]
        go Fail              = []
        go (Error s e)       = [f s e]
        go (Backtrace b s e) = f s e : go b


{- | Case analysis elimination function for the 'ParseError' type. -}
withParseError
    :: a
    -> (s -> e -> a)
    -> (forall d . ParseError s d -> s -> e -> a)
    -> ParseError s e
    -> a
withParseError x _ _ Fail              = x
withParseError _ f _ (Error s e)       = f s e
withParseError _ _ g (Backtrace b s e) = g b s e

{- | The universal property of the initial monoid @'ParseError' s 'Void'@. -}
initial :: Monoid m => ParseError s Void -> m
initial e =
    case e of
        Fail -> mempty
