module Lesson28.Min where

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)


readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt


main :: IO ()
main = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")


-- 
v1 = Just 10
v2 = Just 3
v3 = Just 6


getValues :: Maybe Int
getValues = minOfThree <$> v1 <*> v2 <*> v3


--
data User = User { name    :: String
                 , gamerId :: Int
                 , score   :: Int
                 } deriving Show


serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001


createUser = User <$> serverUsername <*> serverGamerId <*> serverScore


main' :: IO ()
main' = do
    putStrLn "Enter a username, gamerID and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user


userWithoutName = User <$> Nothing <*> serverGamerId <*> serverScore

