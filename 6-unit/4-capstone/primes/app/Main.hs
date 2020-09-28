module Main where

import Primes ( isPrime )

main :: IO ()
main = do
  putStrLn "Enter a number to check if it's prime:"
  input <- getLine
  let n = read input
  case isPrime n of
    Nothing -> print "Sorry, this number is not a valid"
    Just _ -> print "It is prime!"

