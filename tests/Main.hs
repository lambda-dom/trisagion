-- Imports.
-- Testing.
import Hedgehog.Main (defaultMain)

-- Package.
import qualified Tests.Types.Counter as Counter (tests)
import qualified Tests.Types.Offset as Offset (tests)
import qualified Tests.Types.ParseError as ParseError (tests)
import qualified Tests.Typeclasses.Builder as Builder (tests)
import qualified Tests.Parser as Parser (tests)
import qualified Tests.Typeclasses.Binary as Binary (tests)


-- Main test driver.
main :: IO ()
main = defaultMain [
    Counter.tests,
    Offset.tests,
    ParseError.tests,
    Builder.tests,
    Parser.tests,
    Binary.tests
    ] 
