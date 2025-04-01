{- |
Module: Trisagion.Parsers.ParseError

Parsers to handle 'Trisagion.Types.ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Error types.
    -- * Handling 'Trisagion.Types.ParseError'.
    throwParseError,
    onParseError,
    capture,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)

-- Package.
import Trisagion.Types.ParseError (makeParseError, makeBacktrace)
import Trisagion.Typeclasses.HasPosition (HasPosition(..))
import Trisagion.Parser (ParserPE, get, throw, catch)


{- | Throw @'Trisagion.Types.ParseError'@ with error tag @e@ and current stream position. -}
throwParseError :: HasPosition s => e -> ParserPE s e Void
throwParseError e = first absurd get >>= throw . flip makeParseError e

{- | Parser that swallows any thrown error as a backtrace for a new error. -}
onParseError
    :: (HasPosition s, Typeable d, Eq d, Show d)
    => e                                -- ^ Error tag of new error.
    -> ParserPE s d a                   -- ^ Parser to run.
    -> ParserPE s e a
onParseError e p =
    catch
        p
        (\ b -> do
            s <- first absurd get
            absurd <$> throw (makeBacktrace b s e))

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
    x <- p -- Can error here. If it errors, the error's input stream will be @s@.
    ...
@
-}
capture :: HasPosition s => ParserPE s e a -> ParserPE s e a
capture p = do
    s <- first absurd get
    first (first (const (position s))) p
