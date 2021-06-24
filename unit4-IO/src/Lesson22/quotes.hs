main :: IO ()
main = do
    putStrLn "Enter a number between 1 - 5: "
    input <- getLine
    let num = read input :: Int
    putStrLn (quotes !! num)
    putStrLn "If you want to quit press `n`. Otherwise press any key"
    input2 <- getLine 
    let choice = if input == "n"
                 then return ()
                 else  
