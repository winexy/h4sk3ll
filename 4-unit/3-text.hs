{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Semigroup

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

tlines :: T.Text -> [T.Text]
tlines text = T.splitOn "\n" text

tunlines :: [T.Text] -> T.Text
tunlines texts = T.intercalate "\n" texts