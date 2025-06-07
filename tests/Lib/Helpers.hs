module Lib.Helpers (
    -- * Testing helpers.
    testSuccess,
    testError,
    testFail,
) where

-- Imports.
-- Testing.
import Test.Hspec (Expectation, shouldBe)

-- Libraries.
import Optics.Core ((%), preview)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Types.ParseError ( ParseError, singleton, nil )
import Trisagion.Streams.Counter (Counter, initialize)
import Trisagion.Parser (Parser, (:+:), parse)
import Trisagion.Types.ErrorItem (errorItem)


{- | Getter for success info on a parsed result. -}
getSuccess :: HasOffset s => e :+: (a, s) -> Maybe (a, Word)
getSuccess =
    either
        (const Nothing)
        (\ (x, xs) -> Just (x, offset xs))

{- | Get error information on parser failure.

note(s):

    * Backtrace is discarded and returns 'Nothing' for failure or end of input errors.
-}
getError :: ParseError e :+: (a, s) -> Maybe (Word, e)
getError = either (preview (singleton % errorItem)) (const Nothing)

{- | Get error info on parser error with failure -}
getFail :: ParseError e :+: (a, s) -> Maybe ()
getFail = either (preview nil) (const Nothing)

{- | Test parser success by testing equality of output and (position of) updated state. -}
testSuccess
    :: (Streamable s, Show a, Eq a)
    => Parser (Counter s) e a           -- ^ Parser to test.
    -> s                                -- ^ Initial input, usually @'String'@.
    -> a                                -- ^ Parsed result.
    -> Word                             -- ^ Position of updated stream.
    -> Expectation
testSuccess p input x n = getSuccess result `shouldBe` Just (x, n)
    where
        result = parse p (initialize input)

{- | Test parser failure by testing equality of error tag and stream position. -}
testError
    :: (Show e, Eq e)
    => Parser (Counter s) (ParseError e) a  -- ^ Parser to test.
    -> s                                    -- ^ Initial input, usually @'String'@.
    -> e                                    -- ^ Error tag.
    -> Word                                 -- ^ Position of stream in thrown error.
    -> Expectation
testError p input err n = getError result `shouldBe` Just (n, err)
    where
        result = parse p (initialize input)

{- | Test parser @Fail@ errors by testing error equality via @'shouldBe'@. -}
testFail
    :: Parser (Counter s) (ParseError e) a
    -> s                                -- ^ Initial input.
    -> Expectation
testFail p input = getFail result `shouldBe` Just ()
    where
        result = parse p (initialize input)
