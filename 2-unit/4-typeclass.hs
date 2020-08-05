data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 
  deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
  show S1 = "I"
  show S2 = "II"
  show S3 = "III"
  show S4 = "IV"
  show S5 = "V"
  show S6 = "VI"

data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where 
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [Name ("Emil", "Ciorian")
        , Name ("Eugene","Thacker")
        , Name ("Friedrich","Nietzsche")]


--- FIVE SIDED DIE ---


data FiveSidedDie = A | B | C | D | E deriving (Show, Enum)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)

instance Eq FiveSidedDie where
  (==) a b = fromEnum a == fromEnum b

instance Ord FiveSidedDie where
  compare a b = compare (fromEnum a) (fromEnum b)