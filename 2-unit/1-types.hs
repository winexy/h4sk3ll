x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99,0.7,0.8]

-- String === [Char]

letters :: [Char]
letters = ['a','b','c']

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

-- Tupples

ageAndHeight ::(Int, Int)
ageAndHeight = (34, 74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")

-- Function Types

double :: Int -> Int
double x = x * 2

half :: Int -> Double
half x = (fromIntegral x) / 2

halve :: Int -> Int
halve x = x `div` 2

printDouble :: Int -> String
printDouble x = show (x * 2)

z :: Int
z = read "6"

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number,street,town)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
  then f n else n

simple :: a -> a 
simple x = x 

makeTriple :: a -> a -> c -> (a,a,c)
makeTriple x y z = (x,y,z)

myHead :: [a] -> a | 
myHead xs = 
  if length xs > 0
  then length !! 0
  else []

myFoldl :: (a -> b -> a) -> a -> [b] -> a 
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x
  
-- myAverage aList = sum aList / length aList