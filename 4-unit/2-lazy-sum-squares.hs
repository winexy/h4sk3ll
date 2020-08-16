main :: IO ()
main = do
  input <- getContents
  let xs = map read (lines input)
  let squares = map (^2) xs
  print (sum squares)
