{-# LANGUAGE OverloadedStrings #-}

module Palindrome(isPalindrome) where

import Data.Char as C (toLower, isSpace, isPunctuation)
import Data.Text as T (Text, filter, map, reverse)

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . C.isPunctuation) text

toLowerCase :: T.Text -> T.Text
toLowerCase text = T.map C.toLower text

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
