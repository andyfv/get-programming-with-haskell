module Lib
    ( myTake
    ) where

import Data.Char (isDigit)

myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM n (x:xs) = x : myTakePM (n - 1) xs

myHead :: [a] -> a
myHead []    = error "empty list"
myHead (x:_) = x

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs 
                              <*> myTakeSafer (n - 1) (Just (tail xs))


primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n > maxN = Nothing
          | otherwise = Just (n `elem` primes)



-- data Either a b = Fail a | Correct b

eitherHead :: [a] -> Either String a 
eitherHead []     = Left "There is no head becuase the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

add = (+) <$> eitherHead intExample <*> eitherHead (tail intExample)


-- 1.
allDigits :: String -> Bool
allDigits val = all (== True) (map isDigit val)


addStrInts :: String -> String -> Either String Int
addStrInts n1 n2 
    | allDigits n1 && allDigits n2 == True = Right (read n1 + read n2)
    | not (allDigits n1 || allDigits n2)   = Left "Both args invalid"
    | not (allDigits n1)                   = Left "First arg invalid"
    | otherwise                             = Left "Second arg invalid"


-- 2. 
safeSucc :: (Eq a, Bounded a, Enum a) => a -> Maybe a
safeSucc n = if n == maxBound
         then Nothing
         else Just (succ n)

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs

safeLast :: [a] -> Either String a
safeLast [] = Left "No elements left"
safeLast xs = if length xs > maxBound
              then Left "Error: using infinite list"
              else Right (last xs)

-- safeLast' :: Int -> [a] -> Either String a
-- safeLast' 0 _ = Left "List exceeds safe bound"
-- safeLast'
