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
import Optics.Core ((%), set, review)
import Data.Tuple.Optics (_1)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Types.ParseError (ParseError, singleton, makeBacktrace, cons)
import Trisagion.Parser (Parser, (:+:), get, throw, catch)
import Trisagion.Types.ErrorItem (errorItem)


{- | The t'ValidationError' error tag type thrown on failed validations. -}
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)
    deriving (Applicative, Monad) via Identity


{- | Throw @'Trisagion.Types.ParseError'@ with error @e@ and offset the current stream offset. -}
{-# INLINE throwParseError #-}
throwParseError :: HasOffset s => e -> Parser s (ParseError e) a
throwParseError err = do
    n <- first absurd (offset <$> get)
    throw $ review (singleton % errorItem) (n, err)

{- | Capture the offset of the input stream at the entry point in case of a thrown error.

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
    -- Capture the offset of the input stream @n@ here.
    ...
    x <- p -- Can throw here. If it throws, the error's offset will be @n@.
    ...
@
-}
{-# INLINE capture #-}
capture :: HasOffset s => Parser s (ParseError e) a -> Parser s (ParseError e) a
capture p = do
    n <- first absurd (offset <$> get)
    first (set (cons % _1 % errorItem % _1) n) p

{- | Parser that swallows any thrown error as a backtrace for a new error.

The offset of the thrown error is the offset of the input stream captured /before/ @p@ runs as in
the 'capture' combinator.
-}
{-# INLINE onParseError #-}
onParseError
    :: (HasOffset s, Typeable d, Eq d, Show d)
    => e                                -- ^ Error tag of new error.
    -> Parser s (ParseError d) a        -- ^ Parser to run.
    -> Parser s (ParseError e) a
onParseError e p = do
    xs <- first absurd get
    catch
        p
        (throw . makeBacktrace xs e)


{- | Run the parser and return the result, validating it. -}
{-# INLINE validate #-}
validate
    :: HasOffset s
    => (a -> d :+: b)                   -- ^ Validator.
    -> Parser s (ParseError e) a        -- ^ Parser to run.
    -> Parser s (ParseError (d :+: e)) b
validate v p = do
    x <- first (fmap Right) p
    case v x of
        Left d  -> throwParseError (Left d)
        Right y -> pure y
