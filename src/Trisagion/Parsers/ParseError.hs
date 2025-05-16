{- |
Module: Trisagion.Parsers.ParseError

Parsers to handle 'Trisagion.Types.ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Error tag types.
    ValidationError,

    -- * Handling 'Trisagion.Types.ParseError' errors.
    throwParseError,
    capture,
    onParseError,

    -- * Validators.
    validate,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity (..))
import Data.Typeable (Typeable)
import Data.Void (absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Optics.Optic ((%))
import Optics.Review (review)

-- Package.
import Trisagion.Types.ParseError (ParseError, singleton, makeBacktrace)
import Trisagion.Parser (Parser, (:+:), get, throw, catch)
import Trisagion.Types.ErrorItem (errorItem)


{- | The t'ValidationError' error tag type thrown on failed validations. -}
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)
    deriving (Applicative, Monad) via Identity


{- | Throw @'Trisagion.Types.ParseError'@ with error @e@ and input stream the current parser state. -}
{-# INLINE throwParseError #-}
throwParseError :: e -> Parser s (ParseError s e) a
throwParseError err = first absurd get >>= throwError . review (singleton % errorItem) . (, err)

{- | Capture the input stream at the entry point in case of a thrown error.

A parser,

@
parser = do
    ...
    x <- p -- Can throw here.
    ...
@

can now be written as:

@
parser = capture $ do
    -- Capture the input stream @s@ here.
    ...
    x <- p -- Can throw here. If it throws, the error's input stream will be @s@.
    ...
@
-}
{-# INLINE capture #-}
capture :: Parser s (ParseError s e) a -> Parser s (ParseError s e) a
capture p = do
    xs <- first absurd get
    first (first (const xs)) p

{- | Parser that swallows any thrown error as a backtrace for a new error.

The input stream of the thrown error is the input stream captured /before/ @p@ runs as in the
'capture' combinator.
-}
{-# INLINE onParseError #-}
onParseError
    :: (Typeable d, Eq d, Show d)
    => e                                -- ^ Error tag of new error.
    -> Parser s (ParseError s d) a      -- ^ Parser to run.
    -> Parser s (ParseError s e) a
onParseError e p = do
    xs <- first absurd get
    catch
        p
        (fmap absurd . throw . makeBacktrace xs e)


{- | Run the parser and return the result, validating it. -}
{-# INLINE validate #-}
validate
    :: (a -> d :+: b)                   -- ^ Validator.
    -> Parser s (ParseError s e) a      -- ^ Parser to run.
    -> Parser s (ParseError s (d :+: e)) b
validate v p = do
    x <- first (fmap Right) p
    case v x of
        Left d  -> throwParseError (Left d)
        Right y -> pure y
