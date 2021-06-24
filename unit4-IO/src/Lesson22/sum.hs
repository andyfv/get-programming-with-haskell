-- module Sum where

import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)


myMain :: IO ()
myMain = do
    args <- mapM (\_ -> getLine) [1 .. 3]
    mapM_ putStrLn args


myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n action = mapM (\_ -> action) [1 .. n]


--
myMain2 :: IO ()
myMain2 = do
    userInput <- getContents
    mapM_ print userInput