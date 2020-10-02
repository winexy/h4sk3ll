import Data.Char(isDigit)

addStrInts :: String -> String -> Either String Int
addStrInts a b = case (all isDigit a, all isDigit b) of
  (True, True) -> Right (read a + read b)
  (True, False) -> Left "First value can't be parsed"
  (False, True) -> Left "Second value can't be parsed"
  (_, _) -> Left "Neither value can be parsed"