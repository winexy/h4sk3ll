module Main where

import Lib

main :: IO ()
main = someFunc

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n-1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM n (x:xs) = x : myTakePM (n-1) xs

maybeHead :: [a] -> Maybe a 
maybeHead [] = Nothing
maybeHead (x:_) = Just x

safeTake :: Int -> Maybe [a] -> Maybe [a] 
safeTake 0 _ = Just []
safeTake n (Just xs) = (:) 
  <$> maybeHead xs
  <*> safeTake (n-1) (Just (tail xs))

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty" 
eitherHead (x:xs) = Right x