-- Imports.
-- Testing.
import Test.Hspec (hspec)

-- Package.
import qualified Tests.Types.ParseError as ParseError (spec)


-- Main test driver.
main :: IO ()
main = hspec $ do
    ParseError.spec
