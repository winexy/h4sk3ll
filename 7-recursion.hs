takeN n xs = takeRec 0 n [] xs
  where 
    takeRec i n acc [] = acc
    takeRec i n acc xs = 
      if i < n
      then takeRec (i + 1) n (acc ++ [xs !! i]) xs
      else acc


myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

sayAmount n = case n of 
  1 -> "one"
  2 -> "two"
  n -> "a bunch"

sayAmountPM 1 = "one"
sayAmountPM 2 = "two"
sayAmountPM n = "a bunch"

isEmpty [] = True 
isEmpty _ = False

myHead (x:xs) = x
myHead [] = error "No head for empty list"

myTail (_:xs) = xs
myTail [] = [] 