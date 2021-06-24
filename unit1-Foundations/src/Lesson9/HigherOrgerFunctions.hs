module Lesson9.HigherOrderFunctions where

import Data.Char (toLower)

myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []        = []
myFilter test (x:xs) = if   test x
                       then x : myFilter test xs
                       else myFilter test xs


myRemove test []     = []
myRemove test (x:xs) = if   test x
                       then myRemove test xs
                       else x : myRemove test xs



myProduct :: [Integer] -> Integer
myProduct = foldl (*) 1


concatAll :: Foldable t => t [Char] -> [Char]
concatAll xs = foldl (++) "" xs


-- fold and map together
sumOfSquares xs = foldl (+) 0 (map (^2) xs)


-- reverse a list
myReverse xs = foldl (\b a -> a : b) [] xs


myFoldl _ init []     = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x



-- elem
myElem x xs = if    length passedTest > 0
              then  True
              else  False
              where passedTest = filter (== x) xs



-- isPalidrome 
isPalindrome :: String -> Bool
isPalindrome str = 
    processedStr == reverse processedStr
    where processedStr  = map toLower removedSpaces
          removedSpaces = filter (/= ' ') str


-- harmonic 
-- harmonic :: Integer -> Integer
harmonic n = foldl (+) 0 series
    where series = map (1.0 /) [1.0 .. n]