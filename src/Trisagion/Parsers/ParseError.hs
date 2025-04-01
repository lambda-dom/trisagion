{- |
Module: Trisagion.Parsers.ParseError

Parsers to handle 'Trisagion.Types.ParseError' errors.
-}

module Trisagion.Parsers.ParseError (
    -- * Error types.
    ValidationError (..),

    -- * Handling 'Trisagion.Types.ParseError'.
    throwParseError,
    onParseError,
    capture,

    -- * Parsers without errors.
    failIff,
    until,

    -- * Validators.
    validate,
    guardWith,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (until)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)

-- Package.
import Trisagion.Types.ParseError (makeParseError, makeBacktrace, initial)
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Parser (Parser, ParserPE, (:+:), get, throw, catch)
import Trisagion.Parsers.Combinators (lookAhead, many)


{- | The @ValidationError@ error type.

Error thrown on failed validations against a predicate.
-}
newtype ValidationError a = ValidationError a
    deriving stock (Eq, Show, Functor)


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
    x <- p -- Can throw here. If it throws, the error's input stream will be @s@.
    ...
@
-}
capture :: HasPosition s => ParserPE s e a -> ParserPE s e a
capture p = do
    s <- first absurd get
    first (first (const (position s))) p


{- | The parser @failIff p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws a @'Trisagion.Types.ParseError'@ if @p@ succeeds.

note(s):

  * This parser can be used to implement the longest match rule -- see 'until'.
-}
failIff :: ParserPE s e a -> ParserPE s Void ()
failIff p =
    first absurd (lookAhead p) >>=
        either
            (const $ pure ())
            (const $ absurd <$> throw mempty)

{- | The parser @'until' end p@ runs @p@ zero or more times until @end@ succeeds. -}
until
    :: ParserPE s e b                   -- ^ Closing parser.
    -> ParserPE s e a                   -- ^ Parser to run.
    -> Parser s Void [a]
until end p = many $ first initial (failIff end) *> p


{- | Run the parser and return the result, validating it. -}
validate
    :: HasPosition s
    => (a -> Either d b)                -- ^ Validator.
    -> ParserPE s e a                   -- ^ Parser to run.
    -> ParserPE s (e :+: d) b
validate v p = capture $ do
    r <- first (fmap Left) p
    either
        (fmap absurd . throwParseError . Right)
        pure
        (v r)

{- | Guard a parser with a monadic post-condition.

A generalization of 'validate', where the post-condition can depend on the parser state. The
post-condition parser is not allowed to throw an error, because first, ideally it should not
consume input and second, the error part of the type signature is already complicated.

An example is to ensure all input was consumed, e. g. @'guardWith' (const eoi) p@.
-}
guardWith
    :: HasPosition s
    => (a -> Parser s Void Bool)        -- ^ Post-condition.
    -> ParserPE s e a                   -- ^ Parser to run.
    -> ParserPE s (ValidationError a :+: e) a
guardWith cond p = capture $ do
    x <- first (fmap Right) p
    b <- first absurd $ cond x
    if b
        then pure x
        else absurd <$> throwParseError (Left (ValidationError x))
