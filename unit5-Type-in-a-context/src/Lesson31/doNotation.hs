

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName =
    askForName                      -- IO ()
    >> getLine                      -- IO String
    >>= return . nameStatement      -- String -> IO String
    >>= putStrLn                    -- String -> IO ()

-- 
maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM pair = pair >>= (\(a0, a1) -> return (max a0 a1))
-- 

helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (nameStatement name)

-- 

helloPerson :: String -> String
helloPerson name = "Hello " ++ " " ++ name ++ "!"

main :: IO ()
main = do
    name <- getLine
    let statement = helloPerson name
    putStrLn statement


desugared :: IO ()
desugared = 
    getLine
    >>= (\name -> 
            (\statement -> putStrLn statement) (helloPerson name)
        )


echo :: IO ()
echo = do
    input <- getLine
    putStrLn input