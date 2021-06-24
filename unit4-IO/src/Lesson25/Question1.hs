module Lesson25.Question1 where

import System.Environment
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E


main :: IO ()
main = do
    args <- getArgs
    let fileName = args !! 0
    file <- B.readFile fileName
    let textBytesLength = B.length file
    let textCharLength  = T.length $ E.decodeUtf8 file
    putStrLn "Number of bytes : "
    print (textBytesLength)
    putStrLn "Number of characters : "
    print (textCharLength)
