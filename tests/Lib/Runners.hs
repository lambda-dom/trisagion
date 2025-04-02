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
import Trisagion.Types.ParseError (ParseError, withParseError)
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Streams.Counter (Counter, initialize)
import Trisagion.Parser (Parser, ParserPE, (:+:), parse)


{- | Get success info on a parsed result. -}
getSuccess :: HasPosition s => e :+: (a, s) -> Maybe (a, PositionOf s)
getSuccess =
    either
        (const Nothing)
        (\ (x, stream) -> Just (x, position stream))

{- | Get error information on parser failure.

note(s):

    * Backtrace is discarded and returns 'Nothing' for a @'Fail' :: 'ParseError'@.
-}

getError :: ParseError (PositionOf s) e :+: (a, s) -> Maybe (e, PositionOf s)
getError =
    either
        (withParseError
            Nothing
            (\ xs err -> Just (err, xs))
            (\ _ xs err -> Just (err, xs)))
        (const Nothing)

{- | Test parser success by testing equality of output and (position of) updated state. -}
testSuccess
    :: (Show a, Eq a)
    => Parser (Counter s) e a           -- ^ Parser to test.
    -> s                                -- ^ Initial input, usually @'String'@.
    -> a                                -- ^ Parsed result.
    -> Word                             -- ^ Position of updated stream.
    -> Expectation
testSuccess p input x s = getSuccess result `shouldBe` Just (x, s)
    where
        result = parse p (initialize input)

{- | Test parser failure by testing equality of error tag and stream position. -}
testError
    :: (Show e, Eq e)
    => ParserPE (Counter s) e a         -- ^ Parser to test.
    -> s                                -- ^ Initial input, usually @'String'@.
    -> e                                -- ^ Error tag.
    -> Word                             -- ^ Position of stream in thrown error.
    -> Expectation
testError p input err pos = getError result `shouldBe` Just (err, pos)
    where
        result = parse p (initialize input)

{- | Test parser @Fail@ errors by testing error equality via @'shouldBe'@. -}
testFail
    :: (Show e, Eq e)
    -- | Parser to test.
    => ParserPE (Counter s) e a
    -> s                                -- ^ Initial input.
    -> Expectation
testFail p input = getError result `shouldBe` Nothing
    where
        result = parse p (initialize input)
