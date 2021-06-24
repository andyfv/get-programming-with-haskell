module TypeClasses where

class Describable a where
    describe :: a -> String


data IceCream = Chocolate | Vanilla
                deriving (Show)



-- 1. :info Word
minBoundWord = minBound :: Word         -- it's an unsigned Int

minBoundInt = minBound :: Int           -- it's a signed Int


-- 2. succ vs inc
succInt = succ maxBound :: Int  -- Throws err, since it's Bounded type
incInt  = inc maxBound :: Int   -- Cycles to the beggining 

inc :: Int -> Int
inc n = n + 1


-- 3. unbounded succ
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc x = if x == maxBound
              then minBound
              else succ x