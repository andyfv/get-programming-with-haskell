module Lib where

import Data.Array.Unboxed
import Data.Array.IArray

someFunc :: IO ()
someFunc = putStrLn "someFunc"



-------------------- List --------------------
aLargeList :: [Int]
aLargeList = [1 .. 10000000]


-- >>> :set +s
-- >>> aLargeList !! 9999999
-- (0.00 secs, 599,024 bytes)
-- 10000000
-- (0.04 secs, 575,784 bytes)
--


-------------------- Array --------------------
aLargeArray :: UArray Int Int
aLargeArray = array (0, 9999999) []


-- >>> :set +s
-- >>> aLargeArray ! 9999999
-- (0.00 secs, 599,792 bytes)
-- 0
-- (0.00 secs, 570,760 bytes)
--


-------------------- Lazy Problems --------------------
aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList


-- >>> :set +s
-- >>> length aLargeListDoubled
-- (0.00 secs, 599,792 bytes)
-- 10000000
-- (1.15 secs, 1,600,576,024 bytes)
--


-------------------- Creating UArray --------------------
zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 9) [(3, True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]



-- QC 42.1
qcArray :: UArray Int Bool
qcArray = array (0, 4) [(1, True), (2, True)]



-------------------- Updating UArray --------------------
beansInBuckets :: UArray Int Int 
beansInBuckets = array (0,3) $ zip [0 .. 3] $ cycle [0]     -- init values to 0


-- >>> beansInBuckets ! 0
-- 0
-- >>> beansInBuckets ! 2
-- 0

updatedBiB :: UArray Int Int 
updatedBiB = beansInBuckets // [(1, 5), (3, 6)]

-- Add two beans to every bucket
-- >>> accum (+) updatedBiB $ zip [0 .. 3] $ cycle [2]
-- array (0,3) [(0,2),(1,7),(2,2),(3,8)]
--

-- QC 42.3) Double the beans in every bucket
-- >>> accum (*) updatedBiB $ zip [0 .. 3] $ cycle [2]
-- array (0,3) [(0,0),(1,10),(2,0),(3,12)]
--

