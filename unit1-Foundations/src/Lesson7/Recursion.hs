module Recursion where


myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail []     = []
myTail _      = error "No tail"


myGCD :: Integral t => t -> t -> t
myGCD a 0 = a
myGCD a b = myGCD b (mod a b)
    