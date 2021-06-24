module Lesson17.Composition where

import Data.List
import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)


-- Semigroup

data Color = Red 
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange 
           | Brown
           | NoColor
           deriving (Eq, Show)


instance Semigroup Color where
    (<>) a NoColor   = a
    (<>) NoColor a   = a
    (<>) Red Blue    = Purple
    (<>) Blue Red    = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red  = Orange
    (<>) Red Yellow  = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a,b]   = Purple
             | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
             | otherwise                                = Brown


instance Monoid Color where
    mempty  = NoColor
    mappend = (<>)


-- Monoids

-- Event
newtype Event = Event String

instance Show Event where
    show (Event s) = s

instance Semigroup Event where
    (<>) (Event e1) (Event e2) = Event (e1 <> "-" <> e2)

instance Monoid Event where
    mempty  = Event ""
    mappend = (<>)


-- Probability
newtype Prob = Prob Double

instance Show Prob where
    show (Prob p) = show p

instance Semigroup Prob where
    (<>) (Prob p1) (Prob p2) = Prob (p1 * p2)

instance Monoid Prob where
    mempty  = Prob 0.0
    mappend = (<>)

data PTable = PTable [Event] [Prob]

createPTable :: [Event] -> [Prob] -> PTable
createPTable events probs = PTable events normalizedProbs
    where totalProbs      = foldr (\(Prob p) acc -> p + acc) 0.0 probs 
          normalizedProbs = map (\(Prob p) -> Prob (p / totalProbs)) probs


showPair :: Event -> Prob -> String
showPair (Event ev) (Prob p) = 
    mconcat [ ev , "|" , show p , "\n" ]


instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvent newProbs
        where 
            newEvent = [e1' <> e2' | e1' <- e1 , e2' <- e2] 
            newProbs = [p1' <> p2' | p1' <- p1 , p2' <- p2]


instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)


coin :: PTable
coin = createPTable 
    [Event "heads", Event "tails"] 
    [Prob 0.5,Prob  0.5]

spinner :: PTable
spinner = createPTable 
    [Event "red",Event "blue",Event "gree"] 
    [ Prob 0.1, Prob 0.2, Prob 0.7]

