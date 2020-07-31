simple x = x

calcChange owed given = (\ change -> 
  if change > 0 
  then change 
  else 0) (given - owed)
  -- if change > 0 then change else 0
  -- where change = given - owed

doublePlusTwo x = doubleX + 2
  where doubleX = x * 2

doublePlusTwoLambda x = (\ doubleX -> doubleX + 2) (x * 2)

doSomething n = 
  if even n then n - 2 else 3 * n + 1

doSomething2 n = 
  if isEven then n - 2 else 3 * n + 1
  where isEven = mod n 2 == 0

doSmth2L n = (\ isEven -> if isEven then n - 2 else 3 * n - 1) (mod n 2 == 0)

-- sumSquareOrSquareSum x y =
--   if sumSquare > squareSum then sumSquare else squareSum 
--   where sumSquare = x^2 * y^2 
--         squareSum = (x + y) ^ 2

sumSquareOrSquareSum x y = 
  let
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2
  in 
    if sumSquare > squareSum
    then sumSquare
    else squareSum

doubleDouble x = (\dubs -> dubs * 2) (x * 2)

overwrite x = (\x -> 4) $ (\x -> 3) $ (\x -> 2) x
overwrite2 x = (\x -> (\x -> (\x -> x) 4) 3) 2

-- counter x = let x = x + 1
--   in 
--     let x = x + 1
--     in
--       x

counter x = (\x -> x + 1) $ (\x -> x + 1) x
counterNested x = (\x -> x + 1) ((\x -> x + 1) x)