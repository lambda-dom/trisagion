module Lib.Helpers (
    -- * Testing helpers.
    testGetSuccess,
    testGetError,
    testGetFail,
) where

-- Imports.
-- Testing.
import Test.Hspec

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Streams.Counter (Counter, initialize)
import Trisagion.Types.Result (Result (..), withResult)
import Trisagion.Types.ParseError (ParseError, withParseError)
import Trisagion.Get (Get, run)


{- | Extract success info on a parsed result. -}
extractSuccess :: HasPosition s => Result s e a -> Maybe (a, PositionOf s)
extractSuccess =
    withResult
        (const Nothing)
        (\ stream res -> Just (res, getPosition stream))

{- | Extract error information on parser failure.

note(s):

    * returns 'Nothing' likewise for the case of a @'Fail' :: 'ParseError'@.
-}
extractError :: HasPosition s => Result s (ParseError s e) a -> Maybe (e, PositionOf s)
extractError =
    withResult
        (withParseError Nothing (\ _ s err -> Just (err, getPosition s)))
        (\ _ _ -> Nothing)


{- | Test parser success by testing equality of output and updated state. -}
testGetSuccess
    :: (Streamable s, Show a, Eq a)
    => Get (Counter s) e  a         -- ^ Parser to test.
    -> s                            -- ^ Initial input, usually @'String'@.
    -> a                            -- ^ Parsed result.
    -> PositionOf (Counter s)       -- ^ Position of updated stream.
    -> Expectation
testGetSuccess p input x s = extractSuccess result `shouldBe` Just (x, s)
    where
        result = run p (initialize input)

{- | Test parser failure by testing equality of error tag and stream position. -}
testGetError
    :: (Streamable s, Show e, Eq e)
    -- | Parser to test.
    => Get (Counter s) (ParseError (Counter s) e) a
    -> s                            -- ^ Initial input, usually @'String'@.
    -> e                            -- ^ Error tag.
    -> PositionOf (Counter s)       -- ^ Position of stream in thrown error.
    -> Expectation
testGetError p input err position = extractError result `shouldBe` Just (err, position)
    where
        result = run p (initialize input)

{- | Test parser @Fail@ errors by testing error equality via @'shouldBe'@. -}
testGetFail
    :: (Streamable s, Show e, Eq e)
    -- | Parser to test.
    => Get (Counter s) (ParseError (Counter s) e) a
    -> s                            -- ^ Parser input
    -> Expectation
testGetFail p input = extractError result `shouldBe` Nothing
    where
        result = run p (initialize input)
