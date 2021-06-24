module Lesson28.Dist where

import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [ ("Arkham",(42.6054,-70.7829))
                          , ("Innsmouth",(42.8250,-70.8150)) 
                          , ("Carcosa",(29.9714,-90.7694)) 
                          ,("New York",(40.7776,-73.9691))
                          ]


-- Haversine Formula
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
    where rlat  = toRadians lat
          rlong = toRadians long 


haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where (rlat1, rlong1) = latLongToRads coords1
          (rlat2, rlong2) = latLongToRads coords2
          dlat  = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^ 2
          c = 2 * atan2 (sqrt a) (sqrt (1 - a))
          earthRadius = 3961.0


printDistance :: Maybe Double -> IO ()
printDistance Nothing     = putStrLn "Error, invalid city entered"
printDistance (Just dist) = putStrLn (show dist ++ " miles")


haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)


addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just n1) (Just n2) = Just (n1 + n2)
addMaybe _ _                 = Nothing


main :: IO ()
main = do
    putStrLn "Enter the starting city name: "
    startInput <- getLine
    putStrLn "Enter the destination city name: "
    destInput <- getLine
    let startCity = Map.lookup startInput locationDB
    let destCity  = Map.lookup destInput locationDB
    let distance  = haversine <$> startCity <*> destCity
    printDistance distance



val1 = Just 10
val2 = Just 5

multiplyMaybe = (*) <$> val1 <*> val2
divideMaybe   = div <$> val1 <*> val2
modMaybe      = mod <$> val1 <*> val2


-- 1. 
haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO ioVal1 ioVal2 = do
    putStrLn "Enter the starting city name:"
    startInput <- ioVal1
    putStrLn "Enter the destination city name:"
    destCity <- ioVal2
    let distance  = haversine startInput destCity
    return distance
     

-- 2.
haversineIO' :: IO LatLong -> IO LatLong -> IO Double
haversineIO' ioVal1 ioVal2 = haversine <$> ioVal1 <*> ioVal2


-- 3.
data RobotPart = RobotPart { name :: String
                           , description :: String 
                           , cost :: Double
                           , count :: Int
                           } deriving Show


-- main' :: IO ()
-- main' = do
--     putStrLn "Enter part1 id:"
--     partNo1 <- getLine
--     putStrLn "Enter part2 id:"
--     partNo2 <- getLine
--     let part1 = Map.lookup (read part1) partsDB
--     let part2 = Map.lookup (read part2) partsDB
--     let cheapest = min <$> (cost <$> part1) <*> (cost <$> part2)
--     printCost cheapest


printCost :: Maybe Double -> IO ()
printCost Nothing     = putStrLn "Item not found"
printCost (Just cost) = print cost