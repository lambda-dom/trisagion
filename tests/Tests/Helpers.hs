module Tests.Helpers (
    -- * Testing helpers.
    testGetSuccess,
) where

-- Imports.
-- Testing.
import Test.Hspec

-- Package.
import Trisagion.Types.Result (Result, withResult)
import Trisagion.Typeclasses.HasPosition (HasPosition (..))
import Trisagion.Streams.Counter (Counter, initialize)
import Trisagion.Get (Get, run)


{- | Extract success info on a parsed result. -}
extractSuccess :: HasPosition s => Result s e a -> Maybe (a, PositionOf s)
extractSuccess =
    withResult
        (const Nothing)
        (\ stream res -> Just (res, getPosition stream))


{- | Test parser success by testing equality of output and updated state. -}
testGetSuccess
    :: HasPosition s
    => Get s e a                    -- ^ Parser to test.
    -> s                            -- ^ Initial input.
    -> a                            -- ^ Parsed result.
    -> PositionOf (Counter s)       -- ^ Position of updated stream.
    -> Expectation
testGetSuccess p input x position = extractSuccess result `shouldBe` Just (x, output)
    where
        result = run p (initialize input)
