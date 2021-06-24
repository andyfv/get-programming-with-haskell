import System.Environment
import Control.Monad
-- import Data.List.Split.Internals

main :: IO ()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    let squares = map (^2) numbers
    print (sum squares)

toInts :: String -> [Int]
toInts = map read . lines



reverser :: IO ()
reverser = do
    input <- getContents
    let reversed = reverse input
    putStrLn reversed


