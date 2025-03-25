module Lib.Runners (
    -- * Testing helper runners.
    testSuccess,
    testError,
    testFail,
) where

-- Imports.
-- Testing.
import Test.Hspec (Expectation, shouldBe)

-- Package.
import Trisagion.Types.Result (Result, withResult)
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Streams.Counter (Counter, initialize)
import Trisagion.Types.ParseError (ParseError, withParseError)
import Trisagion.Parser (Parser, run)


{- | Get success info on a parsed result. -}
getSuccess :: HasPosition s => Result s e a -> Maybe (a, PositionOf s)
getSuccess =
    withResult
        (const Nothing)
        (\ res stream -> Just (res, getPosition stream))

{- | Get error information on parser failure.

note(s):

    * Backtrace is discarded and returns 'Nothing' for a @'Fail' :: 'ParseError'@.
-}

getError :: HasPosition s => Result s (ParseError s e) a -> Maybe (e, PositionOf s)
getError =
    withResult
        (withParseError
            Nothing
            (\ stream err -> Just (err, getPosition stream))
            (\ _ stream err -> Just (err, getPosition stream)))
        (\ _ _ -> Nothing)

{- | Test parser success by testing equality of output and (position of) updated state. -}
testSuccess
    :: (Show a, Eq a)
    => Parser (Counter s) e  a          -- ^ Parser to test.
    -> s                                -- ^ Initial input, usually @'String'@.
    -> a                                -- ^ Parsed result.
    -> PositionOf (Counter s)           -- ^ Position of updated stream.
    -> Expectation
testSuccess p input x s = getSuccess result `shouldBe` Just (x, s)
    where
        result = run p (initialize input)

{- | Test parser failure by testing equality of error tag and stream position. -}
testError
    :: (Show e, Eq e)
    -- | Parser to test.
    => Parser (Counter s) (ParseError (Counter s) e) a
    -> s                                -- ^ Initial input, usually @'String'@.
    -> e                                -- ^ Error tag.
    -> PositionOf (Counter s)           -- ^ Position of stream in thrown error.
    -> Expectation
testError p input err position = getError result `shouldBe` Just (err, position)
    where
        result = run p (initialize input)

{- | Test parser @Fail@ errors by testing error equality via @'shouldBe'@. -}
testFail
    :: (Show e, Eq e)
    -- | Parser to test.
    => Parser (Counter s) (ParseError (Counter s) e) a
    -> s                                -- ^ Parser input
    -> Expectation
testFail p input = getError result `shouldBe` Nothing
    where
        result = run p (initialize input)
