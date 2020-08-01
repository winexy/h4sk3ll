import Data.Char 

forEach f [] = []
forEach f (x:xs) = f x : forEach f xs


addAnA [] = []
addAnA (x:xs) = ("a " ++ x):addAnA xs

squareAll [] = []
squareAll (x:xs) = x^2:squareAll xs

myFilter predicate [] = []
myFilter predicate (x:xs) = 
  if predicate x 
    then x:myFilter predicate xs
    else myFilter predicate xs

remove predicate [] = []
remove predicate (x:xs) =
  if predicate x
    then remove predicate xs
    else x:remove predicate xs


reduce f init [] = init
reduce f init (x:xs) = reduce f (f init x) xs

elem' x xs = (length filtered) > 0
  where filtered = filter (\y -> y == x) xs

isPalindrome word = processedText == reverse processedText
  where 
    noSpaces = filter (/= ' ') word
    processedText = map toLower noSpaces

harmonic n = sum (take n seriesValues)
  where 
    seriesPairs = zip (cycle [1.0])  [1.0,2.0 .. ]
    seriesValues = map (\pair -> (fst pair)/(snd pair)) seriesPairs