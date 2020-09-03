mystery1 :: Int -> Int -> Int
mystery1 val1 val2 = (val1 + val2 + val3)^2
  where val3 = 3

mystery2 :: Int -> Int -> IO Int
mystery2 val1 val2 = do
  putStrLn "Enter a number" 
  val3Input <- getLine
  let val3 = read val3Input 
  return ((val1 + val2 + val3)^2)

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

sayHello :: IO ()
sayHello = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn (greet name)
