module Main where

import Data.Array.Unboxed
import Data.Array.ST 
import Control.Monad 
import Control.Monad.ST

main :: IO ()
main = print "array"

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]

qcArray :: UArray Int Bool
qcArray = array (0, 4) [(0, True), (2, True), (3, True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) $ zip [0..3] $ cycle [0]
  
listToSTUArray :: [Int] -> ST s (STUArray s Int Int) 
listToSTUArray vals = do
  let end = length vals - 1 
  myArray <- newArray (0,end) 0 
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

myData :: UArray Int Int
myData = listToUArray [7,6,4,8,10,2]

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


crossover :: Int -> UArray Int Int -> UArray Int Int -> UArray Int Int
crossover point left right = runSTUArray $ do
  stArrayL <- thaw left
  let end = (snd . bounds) left
  forM_ [point..end] $ \i -> do
    writeArray stArrayL i $ right point
  return stArrayL

replaceZero :: UArray Int Int -> UArray Int Int
replaceZero xs = runSTUArray $ do
  stArray <- thaw xs
  let end = (snd . bounds) xs
  forM_ [0..end] $ \i -> do
    val <- readArray stArray i
    when (val == 0) $ do
      writeArray stArray i (-1)
  return stArray
    