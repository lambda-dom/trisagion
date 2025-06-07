-- Imports.
-- Testing.
import Hedgehog.Main (defaultMain)

-- Package.
import qualified Tests.Typeclasses.Streamable as Streamable (tests)
import qualified Tests.Types.ParseError as ParseError (tests)


-- Main test driver.
main :: IO ()
main = defaultMain [
    Streamable.tests,
    ParseError.tests
    ] 
