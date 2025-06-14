-- Imports.
-- Testing.
import Hedgehog.Main (defaultMain)

-- Package.
import qualified Tests.Types.Counter as Counter (tests)
import qualified Tests.Types.Offset as Offset (tests)
import qualified Tests.Types.ParseError as ParseError (tests)


-- Main test driver.
main :: IO ()
main = defaultMain [
    Counter.tests,
    Offset.tests,
    ParseError.tests
    ] 
