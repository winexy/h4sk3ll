toInts :: String -> [Int]
toInts text = map read (lines text)
 
main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)