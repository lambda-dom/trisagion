-- Imports.
-- Testing.
import Hedgehog.Main (defaultMain)

-- Package.
import qualified Tests.Typeclasses.Streamable as Streamable (tests)
import qualified Tests.Typeclasses.Splittable as Splittable (tests)
import qualified Tests.Types.ParseError as ParseError (tests)
import qualified Tests.Parser as Parser (tests)


-- Main test driver.
main :: IO ()
main = defaultMain [
    Streamable.tests,
    Splittable.tests,
    ParseError.tests,
    Parser.tests
    ] 
