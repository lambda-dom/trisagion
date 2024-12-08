module Tests.Helpers (
    -- * Test helpers.
    testSuccess,
    testError,
    testFail,

    -- * Table parsers test helpers.
    testTableSuccess,
    testTableError,
) where

-- Imports.
-- Testing.
import Test.Hspec

-- Base.
import Data.Maybe (listToMaybe)
import Data.Void (Void)

-- Libraries.
import Data.Text (Text)
import qualified Data.Text as Text (pack, null)

-- Package.
import Trisagion.Types.Result (Result (..), withResult)
import Trisagion.Types.ParseError (ParseError (..), getTag, getState)
import Trisagion.Streams.Streamable (getStream, getOffset)
import Trisagion.Get (Get, run)
import Trisagion.Examples.Table (Lines, initLines)
import Data.Bifunctor (Bifunctor(..))

 
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

{- | Extract data for success results of @'Get' 'Lines'@ parsers. -}
extractSuccess :: Result Lines e a -> Maybe (a, (Word, Maybe Text))
extractSuccess =
    withResult
        (const Nothing)
        (\ ls x -> Just (x, (getOffset ls, listToMaybe $ getStream ls)))

{- | Extract data for error results of @'Get' 'Lines'@ parsers. -}
extractParseError :: Result Lines (ParseError Lines e) a -> Maybe (e, (Word, Maybe Text))
extractParseError = withResult processError (\ _ _ -> Nothing)
    where
        processError :: ParseError Lines e -> Maybe (e, (Word, Maybe Text))
        processError err =
            let mpair = liftA2 (,) (getTag err) (getState err) in
                second (\ ls -> (getOffset ls, listToMaybe $ getStream ls)) <$> mpair

{- | Test parser success by testing success equality via @'shouldBe'@. -}
testTableSuccess
    :: (Show a, Eq a)
    => Get Lines e a    -- ^ Parser to test.
    -> String           -- ^ Parser input. 
    -> a                -- ^ Parsed result.
    -> (Word, String)   -- ^ Stream position and first line of updated state.
    -> Expectation
testTableSuccess p input ok (offset, out) =
        let r = extractSuccess $ run p (initLines $ Text.pack input) in
            r `shouldBe` Just (ok, (offset, justIfNotNull $ Text.pack out))
    where
        justIfNotNull text = if Text.null text then Nothing else Just text

{- | Test table parser errors by testing error equality via @'shouldBe'@. -}
testTableError
    :: (Show e, Eq e)
    => Get Lines (ParseError Lines e) a     -- ^ Parser to test.
    -> String                               -- ^ Parser input.
    -> e                                    -- ^ Error tag.
    -> (Word, String)                       -- ^ Error state component.
    -> Expectation
testTableError p input err (offset, out) =
        let r = extractParseError $ run p (initLines $ Text.pack input) in
            r `shouldBe` Just (err, (offset, justIfNotNull $ Text.pack out))
    where
        justIfNotNull text = if Text.null text then Nothing else Just text
