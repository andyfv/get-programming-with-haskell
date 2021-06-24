module Pizza where

import Data.Map as Map

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2


type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = 
    if costP1 < costP2
        then p1
        else p2
    where costP1 = costPerInch p1
          costP2 = costPerInch p2
                    

describePizza :: Pizza -> String
describePizza p@(size, _) = 
    "The " 
    ++ show size 
    ++ " pizza "
    ++ "is cheaper at " 
    ++ show costSqInch
    ++ " per square inch"
    where costSqInch = costPerInch p


main :: IO ()
main = do
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <- getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)


costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)


-- 1.
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

names :: Map.Map Int String
names = Map.fromList [(1,"Andy")]

maybeMain' :: Maybe String
maybeMain' = do
    name <- Map.lookup 1 names
    let greeting = helloPerson name
    return (greeting)


-- 2. 
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 2
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)


fastFib' :: Integer -> Integer
fastFib' n = fastFib 1 1 n


fibMain :: IO ()
fibMain = do
    putStrLn "Enter a number : "
    input <- getLine
    let number = read input
    let result = fastFib' number
    putStrLn ("The result is : " ++ show result)
