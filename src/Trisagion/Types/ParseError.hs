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
    ParseError
        :: (Typeable d, Eq d, Show d)
        => !(Maybe (ParseError s d))    -- ^ Backtrace.
        -> !s                           -- ^ Input stream.
        -> !e                           -- ^ Error tag.
        -> ParseError s e


-- Instances.
deriving stock instance (Show s, Show e) => Show (ParseError s e)
deriving stock instance Functor (ParseError s)

{- | The 'Bifunctor' instance.

A typical use case is, for input streams with a notion of position (see 'Trisagion.Typeclasses.HasPosition.HasPosition'),
to capture the position of the stream instead of the stream itself:

@
capture :: 'ParseError' s e -> 'ParseError' (PositionOf s) e
capture = first getPosition
@
-}
instance Bifunctor ParseError where
    bimap :: forall s t d e . (s -> t) -> (d -> e) -> ParseError s d -> ParseError t e
    bimap _ _ Fail               = Fail
    bimap f g (ParseError b s e) = ParseError (go <$> b) (f s) (g e)
        where
            go :: ParseError s c -> ParseError t c
            go Fail                      = Fail
            go (ParseError bt state err) = ParseError (go <$> bt) (f state) err

instance (Eq s, Eq e) => Eq (ParseError s e) where
    (==) :: ParseError s e -> ParseError s e -> Bool
    (==) (ParseError (b :: Maybe (ParseError s d)) s e ) (ParseError  (b' :: Maybe (ParseError s d')) s' e') =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e' && s == s' && b == b'
    (==) Fail Fail = True
    (==) _    _    = False

instance Semigroup (ParseError s e) where
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Fail x = x
    (<>) x    _ = x

instance Monoid (ParseError s e) where
    mempty :: ParseError s e
    mempty = Fail


{- | Constructor helper to create 'ParseError' values.

note(s):

    * Since a @Fail@ backtrace is functionally equivalent to no backtrace, 'makeParseError'
    normalizes it.
-}
makeParseError
    :: (Typeable d, Eq d, Show d)
    => ParseError s d                   -- ^ Backtrace.
    -> s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError s e
makeParseError b = ParseError (go b)
    where
        go :: ParseError s d -> Maybe (ParseError s d)
        go r = case r of
            (ParseError {}) -> Just r
            Fail            -> Nothing

{- | Constructor helper to create a 'ParseError' value with no backtrace.

note(s):

    * The backtrace is a @'Nothing'@ of type @'Maybe' ('ParseError' s 'Void')@.
-}
makeParseErrorNoBacktrace
    :: s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError s e
makeParseErrorNoBacktrace =
    let b = Nothing :: Maybe (ParseError s Void) in
        ParseError b


{- | Prism for the state component. -}
getState :: ParseError s e -> Maybe s
getState Fail               = Nothing
getState (ParseError _ s _) = Just s

{- | Prism for the error tag. -}
getTag :: ParseError s e -> Maybe e
getTag Fail               = Nothing
getTag (ParseError _ _ e) = Just e

{- | Getter for the backtrace of an error as an elimination function. -}
getBacktrace :: forall s e a . (forall d . s -> d -> a) -> ParseError s e -> [a]
getBacktrace f = go
    where
        go :: ParseError s c -> [a]
        go Fail               = []
        go (ParseError b s e) = f s e : maybe [] go b


{- | The universal property of the initial monoid @'ParseError' s 'Void'@. -}
initial :: Monoid m => ParseError s Void -> m
initial e =
    case e of
        Fail -> mempty
