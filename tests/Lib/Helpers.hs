module Lib.Helpers (
    -- * Testing helpers.
    testGetSuccess,
    testGetError,
) where

-- Imports.
-- Testing.
import Test.Hspec

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Streams.Counter (Counter, initialize)
import Trisagion.Types.Result (Result, withResult)
import Trisagion.Types.ParseError (ParseError, withParseError)
import Trisagion.Get (Get, run)
import Trisagion.Getters.ParseError (GetPE)


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
extractError :: Result s (ParseError (PositionOf s) e) a -> Maybe (e, PositionOf s)
extractError =
    withResult
        (withParseError Nothing (\ _ s err -> Just (err, s)))
        (\ _ _ -> Nothing)


{- | Test parser success by testing equality of output and updated state. -}
testGetSuccess
    :: (Streamable s, Show a, Eq a)
    => Get (Counter s) e  a         -- ^ Parser to test.
    -> s                            -- ^ Initial input, usually @'String'@.
    -> a                            -- ^ Parsed result.
    -> PositionOf (Counter s)       -- ^ Position of updated stream.
    -> Expectation
testGetSuccess p input x position = extractSuccess result `shouldBe` Just (x, position)
    where
        result = run p (initialize input)

{- | Test parser failure by testing equality of error tag and stream position. -}
testGetError
    :: (Show e, Eq e)
    => GetPE (Counter s) e a        -- ^ Parser to test.
    -> s                            -- ^ Initial input, usually @'String'@.
    -> e                            -- ^ Error tag.
    -> PositionOf (Counter s)       -- ^ Position of stream in thrown error.
    -> Expectation
testGetError p input err position = extractError result `shouldBe` Just (err, position)
    where
        result = run p (initialize input)
