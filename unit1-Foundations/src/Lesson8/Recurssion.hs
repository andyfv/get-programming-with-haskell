module Recursion where


-- 1. length
myLenght :: Num p => [a] -> p
myLenght [] = 0
myLenght (_:xs) = 1 + myLenght xs


-- 2. take
myTake :: Int -> [a] -> [a]
myTake _ []     = []
myTake 0 _      = []
myTake n (x:xs) = x : rest
    where rest = myTake (n - 1) xs


-- 3. cycle
myCycle (x:xs) = x : myCycle(xs ++ [x])


-- 4. ackermann
ackermann :: (Num a, Num t, Eq a, Eq t) => a -> t -> t
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))


-- 5. collatz
-- collatz :: (Num a2, Integral a1, Fractional a1) => a1 -> a2
collatz 1 = 1
collatz n = 
    if   even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)


-- 6. reverse
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x]


-- 7. fibonacci
fibonacci :: (Eq a, Num a, Num p) => a -> p
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)



-- 8. fastFib
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 2
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)