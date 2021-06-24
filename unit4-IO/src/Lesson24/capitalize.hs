import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO



main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    fileContent <- TIO.readFile fileName
    TIO.writeFile fileName (T.toUpper fileContent)

