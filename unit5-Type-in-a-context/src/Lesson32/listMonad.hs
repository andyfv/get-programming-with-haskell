
import Control.Monad
import Data.Char

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2 ^ value)


powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2^value
    let powersOfThree = 3^value
    return (powersOfTwo, powersOfThree)
    

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenValue <- [2,4 .. n]
    oddValue  <- [1,3 .. n]
    return (evenValue, oddValue)


squares10 :: [(Int, Int)]
squares10 = do
    val <- [1 .. 10]
    return (val, val^2)


------ Control.Monad (guard) --------
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard(even value)
    return value

-- 
guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter test vals = do
    val <- vals
    guard(test val)
    return val


evenSquares :: [Int]
evenSquares = do
    n <- [0 .. 9]
    let nSquared = n^2
    guard(even nSquared)
    return nSquared


capitalizeWords :: [String] -> [String]
capitalizeWords vals = ["Mr" ++ capVal | val <- vals
                                       , let capVal = (\(x:xs) -> toUpper x:xs) val
                                       ]


-- 1. Calendar Dates
days :: [Int]
days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates nums = [date | num <- nums, date <- [1 .. num]]

-- 2. 
datesDo :: [Int] -> [Int]
datesDo nums = do
    num  <- nums 
    date <- [1 .. num]
    return date


datesMonad :: [Int] -> [Int]
datesMonad nums = 
    nums 
    >>= (\num -> 
            [1 .. num] 
            >>= (\date -> return date)
        )
