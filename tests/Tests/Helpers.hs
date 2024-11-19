module Tests.Helpers (
    -- * Test helpers.
    testSuccess,
    testError,
    testFail,
) where

-- Imports.
-- Testing.
import Test.Hspec

-- Libraries.
import Data.Void (Void)

-- Package.
import Trisagion.Types.Result (Result(..))
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Get (Get, run)

 
{- | Test parser success by testing success equality via @'shouldBe'@. -}
testSuccess
    :: (Show s, Show e, Show a, Eq s, Eq e, Eq a)
    => Get s e a        -- ^ Parser to test.
    -> s                -- ^ Parser input. 
    -> a                -- ^ Parsed result.
    -> s                -- ^ Updated state.
    -> Expectation
testSuccess p input x output = run p input `shouldBe` Success x output

{- | Test parser errors by testing error equality via @'shouldBe'@. -}
testError
    :: (Show s, Show e, Show a, Eq s, Eq e, Eq a)
    => Get s (ParseError s e) a     -- ^ Parser to test.
    -> s                            -- ^ Parser input.
    -> s                            -- ^ Error state component.
    -> e                            -- ^ Error tag.
    -> Expectation
testError p input output tag = run p input `shouldBe` Error err
    where
        err = ParseError (Nothing :: Maybe (ParseError s Void)) output tag

{- | Test parser 'Fail' errors by testing error equality via @'shouldBe'@. -}
testFail
    :: (Show s, Show e, Show a, Eq s, Eq e, Eq a)
    => Get s (ParseError s e) a     -- ^ Parser to test.
    -> s                            -- ^ Parser input
    -> Expectation
testFail p input = run p input `shouldBe` Error Fail
