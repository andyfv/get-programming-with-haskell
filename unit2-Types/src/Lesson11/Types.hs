module Lesson11.Types where

half :: Int -> Double
half n = (fromIntegral n) / 2


halve :: Int -> Int
halve n = 5 `div` 2


printDouble :: Int -> String
printDouble n = show (n * 2)


myHead :: [a] -> a
-- myHead []     = [] <- This won't work
myHead (x:_) = x

myTail :: [a] -> [a]
myTail []     = []
myTail (_:xs) = xs