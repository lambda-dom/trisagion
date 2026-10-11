-- Imports.
-- Testing.
import Test.Tasty (defaultMain, testGroup)

-- Package.


-- Main test driver.
main :: IO ()
main =
    defaultMain $
        testGroup
        "Package tests."
        []
