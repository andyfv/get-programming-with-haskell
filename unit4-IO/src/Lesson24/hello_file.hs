
import System.IO    -- allows to read and write files

main :: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    hasLine   <- hIsEOF helloFile
    firstLine <- if not hasLine
                 then hGetLine helloFile
                 else return "empty"
    putStrLn "done!"
    