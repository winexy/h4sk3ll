fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = 
  if n < 0 
    then error "invalid number"
    else fib (n-1) + fib (n - 2)


main :: IO ()
main = do
  putStrLn "Input n: "
  n <- getLine
  let nth = fib (read n)
  putStrLn ("Nth fibonacci is: " ++ show nth)