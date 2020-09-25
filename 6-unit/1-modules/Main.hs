module Main where

import Data.Text.IO as T (getLine)
import Palindrome (isPalindrome)

main :: IO ()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- T.getLine
  let response = if Palindrome.isPalindrome text
                  then "it is"
                  else "it's not"
  print response                  