class Describable a where
  describe :: a -> String


data Icecream = Vanilla | Chocolate deriving (Show, Eq, Ord)

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound then minBound else succ n