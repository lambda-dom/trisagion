-- Imports.
-- Testing.
import Test.Tasty (defaultMain, testGroup)

-- Package.
import qualified Tests.Types.ParseError as ParseError (tests)


-- Main test driver.
main :: IO ()
main =
    defaultMain $
        testGroup
        "Package tests."
        [
            ParseError.tests
        ]
