dropN _ [] = []
dropN 0 xs = xs
dropN n (_:xs) = dropN (n - 1) xs

len [] = 0
len (x:xs) = 1 + len xs

takeN _ [] = []
takeN 0 _ = []
takeN n (x:xs) = x:rest
  where rest = takeN (n - 1) xs


finiteCycle (first:rest) = first:rest ++ [first]

myCycle (first:rest) = first:myCycle (rest++[first])

ackerman 0 n = n + 1
ackerman m 0 = ackerman (m-1) 1
ackerman m n = ackerman (m-1) (ackerman m (n-1))


collatz 1 = 1
collatz n = if even n 
  then 1 + collatz (n `div` 2) 
  else 1 + collatz (n * 3 + 1)


reverseIt [] = []
reverseIt (x:[]) = [x]
reverseIt (x:xs) = (reverseIt xs) ++ [x]

fib n = fastAndFibonacci 1 1 n
  where
    fastAndFibonacci _ _ 0 = 0
    fastAndFibonacci _ _ 1 = 1
    fastAndFibonacci _ _ 2 = 1
    fastAndFibonacci x y 3 = x + y
    fastAndFibonacci x y c = 
      if c < 0
      then error "negative number"
      else fastAndFibonacci (x + y) x (c - 1)
