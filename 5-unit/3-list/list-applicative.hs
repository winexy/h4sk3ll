doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxMultiplier :: [Int]
boxMultiplier = [10,50]

newOutcomes :: [Int]
newOutcomes = pure (*) <*> doorPrize <*> boxMultiplier