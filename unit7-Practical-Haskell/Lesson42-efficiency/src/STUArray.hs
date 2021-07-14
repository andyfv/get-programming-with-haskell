module STUArray where

import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import Control.Monad 
import Control.Monad.ST

-------------------- Mutating State with STUArray --------------------
listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1           -- end bound
    stArray <- newArray (0, end) 0      
    return stArray                      



listToSTUArray' :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray' vals = do
    let end = length vals - 1           -- end bound
    myArray <- newArray (0, end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray


-- 
listToUArray :: [Int] -> UArray Int Int 
listToUArray vals = runSTUArray $ listToSTUArray vals

listToUArray' :: [Int] -> UArray Int Int
listToUArray' vals = runSTUArray $ do
    let end = length vals - 1
    myArray <- newArray (0, end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray


--------------------- ST Type ---------------------
swapST :: (Int, Int) -> (Int, Int)
swapST (x,y) = runST $ do 
    x' <- newSTRef x
    y' <- newSTRef y
    writeSTRef x' y
    writeSTRef y' x
    xfinal <- readSTRef x'
    yfinal <- readSTRef y'
    return (xfinal, yfinal)


--------------------- Bubble Sort ---------------------
myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]

-- QC 42.4
myData' :: UArray Int Int
myData' = listToUArray [7,6,4,8,10,2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j + 1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j + 1) val
    return stArray


-- 1. Crossover
crossOver :: (UArray Int Int, UArray Int Int) -> Int -> UArray Int Int
crossOver (a1, a2) crossOverPt = runSTUArray $ do
    st1 <- thaw a1
    let end = (snd . bounds) a1
    forM_ [crossOverPt .. end] $ \i -> do
        writeArray st1 i (a2 ! i)
    return st1


-- 2.
replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros vals = runSTUArray $ do
    stArray <- thaw vals
    let (begin, end) = bounds vals
    forM_ [begin .. end] $ \i -> do
        val <- readArray stArray i
        when (val == 0) $ do 
            writeArray stArray i (-1)
    return stArray


zerosAndOnes :: UArray Int Int
zerosAndOnes = listToUArray [0, 0, 1, 1, 0, 1, 0]