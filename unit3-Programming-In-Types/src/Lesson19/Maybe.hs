module Lesson19.Maybe where

import Data.Maybe
import Data.List
import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen
             deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs 

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs


possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawersContens :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawersContens ids catalog = map getContents ids
    where getContents = \id -> Map.lookup id catalog


availableOrgans :: [Maybe Organ]
availableOrgans = getDrawersContens possibleDrawers organCatalog


countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = 
    length (filter (\x -> x == Just organ) available)


isSomething :: Maybe Organ -> Bool
isSomething Nothing  = False
isSomething (Just _) = True


justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans


showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""


organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList


numOrZero :: Maybe Int -> Int
numOrZero (Just n) = n
numOrZero Nothing  = 0

-- 

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ)    = show organ ++ " in a vet"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ)    = show organ ++ " in a bag"


data Location = Lab | Kitchen | Bathroom 
                deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ


placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a)    = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a)    = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) = 
    show container ++ " in the " ++ show location


processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndRequest organ
    where organ = Map.lookup id catalog


processAndRequest :: (Maybe Organ) -> String
processAndRequest (Just organ) = report (process organ)
processAndRequest Nothing      = "error, id not found"


-- 1. Tells you the number of empty drawers
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . filter isNothing

-- 2. maybeMap
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just a) = Just (f a)
maybeMap _ Nothing  = Nothing