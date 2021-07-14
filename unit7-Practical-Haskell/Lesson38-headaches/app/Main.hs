module Main where

import Lib
import Prime

main :: IO ()
main = do
    print "Enter a number to test for primality: "
    n <- read <$> getLine
    let result = isPrime n
    print (displayResult result)
