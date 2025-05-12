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
    stream,
    tag,
    backtrace,

    -- * Elimination functions.
    withParseError,
    initial,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable, type (:~:) (Refl), eqT)
import Data.Void (Void)

-- Package.
import Trisagion.Typeclasses.HasPosition (HasPosition (..))


{- | The @ParseError s e@ error type. -}
data ParseError s e where
    -- | Error with no indication of its origin. The monoid unit for 'ParseError'.
    Fail :: ParseError s e

    -- | Error without a backtrace.
    Error
        :: !s                           -- ^ Input stream.
        -> !e                           -- ^ Error tag.
        -> ParseError s e

    -- | Error with a backtrace.
    Backtrace
        :: (Typeable d, Eq d, Show d)
        => ParseError s d               -- ^ Backtrace. Lazy for list-like behavior.
        -> !s                           -- ^ Input stream.
        -> !e                           -- ^ Error tag.
        -> ParseError s e

-- Instances.
deriving stock instance (Show s, Show e) => Show (ParseError s e)
deriving stock instance Functor (ParseError s)

instance (Eq s, Eq e) => Eq (ParseError s e) where
    (==) :: ParseError s e -> ParseError s e -> Bool
    (==) Fail Fail = True
    (==) (Error s e) (Error s' e') = e == e' && s == s'
    (==) (Backtrace (b :: ParseError s d) s e ) (Backtrace (b' :: ParseError s d') s' e') =
        case eqT @d @d' of
            Nothing   -> False
            Just Refl -> e == e' && s == s' && b == b'
    (==) _ _ = False

{- | The 'Bifunctor' instance. -}
instance Bifunctor ParseError where
    bimap :: forall s t d e . (s -> t) -> (d -> e) -> ParseError s d -> ParseError t e
    bimap _ _ Fail              = Fail
    bimap f g (Error s e)       = Error (f s) (g e)
    bimap f g (Backtrace b s e) = Backtrace (first f b) (f s) (g e)

instance Semigroup (ParseError s e) where
    (<>) :: ParseError s e -> ParseError s e -> ParseError s e
    (<>) Fail x = x
    (<>) x    _ = x

instance Monoid (ParseError s e) where
    mempty :: ParseError s e
    mempty = Fail


{- | Construct a 'ParseError' value with no backtrace. -}
makeParseError
    :: HasPosition s
    => s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError (PositionOf s) e
makeParseError s = Error (position s)

{- | Construct a 'ParseError' value with a backtrace.

note(s):

    * Since a @Fail@ backtrace is functionally equivalent to no backtrace, 'makeBacktrace'
    normalizes it.
-}
makeBacktrace
    :: forall d s e . (HasPosition s, Typeable d, Eq d, Show d)
    => ParseError (PositionOf s) d      -- ^ Backtrace.
    -> s                                -- ^ Input stream.
    -> e                                -- ^ Error tag.
    -> ParseError (PositionOf s) e
makeBacktrace b s =
    case b of
        Fail -> Error (position s)
        _    -> Backtrace b (position s)


{- | Prism for the input stream component. -}
stream :: ParseError s e -> Maybe s
stream Fail              = Nothing
stream (Error s _)       = Just s
stream (Backtrace _ s _) = Just s

{- | Prism for the error tag. -}
tag :: ParseError s e -> Maybe e
tag Fail              = Nothing
tag (Error _ e)       = Just e
tag (Backtrace _ _ e) = Just e

{- | Prism for the backtrace of an error as an elimination function. -}
backtrace :: forall s e a . (forall d . s -> d -> a) -> ParseError s e -> [a]
backtrace f = go
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
