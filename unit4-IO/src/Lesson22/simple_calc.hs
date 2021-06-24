import System.Environment


main :: IO ()
main = do
    input <- getContents
    let values = lines input
    print (calc values)

calc :: [String] -> Int
calc (val1:"+":val2:_) = read val1 + read val2
calc (val1:"*":val2:_) = read val1 * read val2

