import Control.Monad
import Data.Char

powerOfTwo :: Int -> [Int]
powerOfTwo n = do
  value <- [1 .. n]
  return (value ^ 2)

powerOfTwo2 :: Int -> [Int]
powerOfTwo2 n = [v ^ 2 | v <- [1..n]]

powersOfTwoAndThree :: Int -> [(Int,Int)] 
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2^value
  let powersOfThree = 3^value
  return (powersOfTwo,powersOfThree)

powersOfTwoAndThree2 :: Int -> [(Int, Int)]
powersOfTwoAndThree2 n = [(pow2, pow3) | v <- [1..n] , let pow2 = v ^ 2, let pow3 = v ^ 3]

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evens <- [2, 4 .. n]
  odds <- [1, 3 .. n]
  return (evens, odds)

pairsSqr :: [(Int, Int)]
pairsSqr = do
  x <- [1..10]
  return (x, x ^ 2)

evensGuard :: Int -> [Int] 
evensGuard n = do
  value <- [1..n]
  guard (even value)
  return value

filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate xs = do
  x <- xs
  guard(predicate x)
  return x  

evensGuard2 :: Int -> [Int]
evensGuard2 n = [x | x <- [1 .. n], even x]


gentlemans :: [String]
gentlemans = ["Mr. " ++ toUpper x:xs | (x:xs) <- ["brown","blue","pink","orange"]]