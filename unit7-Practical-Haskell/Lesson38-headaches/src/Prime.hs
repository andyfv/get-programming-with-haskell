
module Prime where

isPrime :: Int -> Either PrimeError Bool
isPrime n | n < 2 = Left InvalidValue
          | n > maxN = Left TooLarge
          | otherwise = Right (n `elem` primes)


primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10


data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
    show TooLarge     = "Value exceed max bound"
    show InvalidValue = "Value is not a valid candidate for prime checking"

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's a prime"
displayResult (Right False) = "It's a composite"
displayResult (Left primeError) = show primeError