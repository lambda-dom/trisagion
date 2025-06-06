{- |
Module: Trisagion.Parsers.ParseError

Parsers for handling t'ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Error parsers.
    throwParseError,
    capture,
    onParseError,
    validate,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)

-- Libraries.
import Control.Monad.State (MonadState (..), gets)
import Optics.Core ((%), review, set, _1)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Types.ErrorItem (errorItem)
import Trisagion.Types.ParseError (ParseError, singleton, cons, makeBacktrace)
import Trisagion.Parser (Parser, (:+:), throw, catch)


-- $setup
-- >>> import Trisagion.Streams.Counter
-- >>> import Trisagion.Parser
-- >>> import Trisagion.Parsers.Streamable


{- | Throw @'Trisagion.Types.ParseError'@ with error tag @e@ and offset the current stream offset. -}
{-# INLINE throwParseError #-}
throwParseError :: HasOffset s => e -> Parser s (ParseError e) a
throwParseError err = do
    n <- gets offset
    throw $ review (singleton % errorItem) (n, err)

{- | Capture the offset of the input stream at the entry point in case of an error.

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
    -- Capture the offset @n@ of the input stream here.
    ...
    x <- p -- Can throw here. If it throws, the error's offset will be @n@.
    ...
@
-}
{-# INLINE capture #-}
capture :: HasOffset s => Parser s (ParseError e) a -> Parser s (ParseError e) a
capture p = do
    n <- gets offset
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
    xs <- get
    catch
        p
        (throw . makeBacktrace xs e)

{- | Run the parser and return the result, validating it.

=== __Examples:__

>>> parse (validate (\ c -> if c == '0' then Right c else Left ()) one) (initialize "0123")
Right ('0',Counter 1 "123")

>>> parse (validate (\ c -> if c == '0' then Right c else Left ()) one) (initialize "123")
Left (Cons (ErrorItem 1 (Left ())) [])

>>> parse (validate (\ c -> if c == '0' then Right c else Left ()) one) (initialize "")
Left (Cons (EndOfInput 1) [])
-}
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
