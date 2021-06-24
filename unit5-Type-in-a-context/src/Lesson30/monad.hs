
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"




helloName :: IO ()
helloName =
    askForName                                  -- IO ()
    >> getLine                                  -- IO String
    >>= return . nameStatement                  -- String -> IO String
    >>= putStrLn                                -- String -> IO ()


-- 
-- (\num -> return (+ 2 num))


-- 1. allFmapM
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func value = value >>= (\x -> return (func x))
                  --  context >>= return . func


-- 2. allApp
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp func value = 
    value 
    >>= (\x -> func 
            >>= (\f -> return (f x))
        )


allApp' :: Monad m => m (a -> b) -> m a -> m b
allApp' func value = 
    func >>= (\f -> value >>= return . f)



-- 3. bind
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _  = Nothing 
bind (Just x) f = f x
