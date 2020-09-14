allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f fa = pure f <*> fa

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) 
  <*> (pure (+) <*> pure 2 <*> pure 4) 
  <*> pure 6

startingBeer :: [Int]
startingBeer = [6, 12]

remainingBeer :: [Int]
remainingBeer = (\count -> count - 4) <$> startingBeer

friends :: [Int]
friends = [2, 3]

totalPeople :: [Int]
totalPeople = (+ 2) <$> friends

beersPerGuest :: [Int]
beersPerGuest = [3, 4]

totalBeersNeeded :: [Int]
totalBeersNeeded = pure (*) <*> beersPerGuest <*> totalPeople

beersToPurchase :: [Int]
beersToPurchase = pure (-) <*> totalBeersNeeded  <*> remainingBeer
