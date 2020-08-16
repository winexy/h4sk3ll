calc :: [String] -> Int
calc (a:"+":b:_) = read a + read b
calc (a:"-":b:_) = read a - read b
calc (a:"*":b:_) = read a * read b


main :: IO ()
main = do
  input <- getContents
  print $ calc $ lines input