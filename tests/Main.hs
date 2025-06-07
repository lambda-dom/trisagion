-- Imports.
-- Testing.
import Hedgehog.Main (defaultMain)

-- Package.
import qualified Tests.Types.ParseError as ParseError (tests)


-- Main test driver.
main :: IO ()
main = defaultMain [
    ParseError.tests
    ] 
