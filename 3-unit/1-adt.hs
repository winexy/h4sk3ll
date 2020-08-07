data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show

data KidsBreakfast = KidsBreakfast (BreakfastMain, BreakfastSide)
data BasicBreakfast = BasicBreakfast (BreakfastMain, BreakfastMeat, BreakfastSide)
data Lumberjack = Lumberjack (BreakfastSide
  , BreakfastMain
  , BreakfastMeat
  , BreakfastSide
  , BreakfastMain
  , BreakfastMeat
  , BreakfastSide)


---

type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName

instance Show Name where
  show (Name fname lname) = fname ++ " " ++ lname
  show (NameWithMiddle fname mname lname) = fname ++ " " ++ mname ++ " " ++ lname
  show (TwoInitialsWithLast fst snd lname) = (fst : ". ") ++ (snd : ". ") ++ lname

  
data Author = Author Name
data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

instance Show Creator where
  show (AuthorCreator (Author name)) = show name
  show (ArtistCreator (Person name)) = show name
  show (ArtistCreator (Band name)) = show name

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book = Book {
    author :: Creator
  , isbn :: String
  , title :: String
  , year :: Int
  , bookPrice :: Double
}

data VinylRecord = VinylRecord {
    artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name :: String
  , description :: String
  , toyPrice :: Double
}

data StoreItem = BookItem Book 
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlets


price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0.0

madeBy :: StoreItem -> String
madeBy item = case item of 
  BookItem book -> show $ author book
  RecordItem record -> show $ artist record
  _ -> "Unkown author"

book = BookItem $ Book {
  author = AuthorCreator (Author (Name "Olzhas" "Seitmagambet"))
  , isbn = "123456789"
  , title = "Adventure Time"
  , year = 1996
  , bookPrice = 100500
}

toy = ToyItem $ CollectibleToy {
  name = "Illidan"
  , description = "u r not prepared"
  , toyPrice = 100.500
}


data Pamphlets = Pamphlets {
  pamphletTitle :: String
  , pamphletDescription :: String
  , pampthletContact :: String
}


---


type Radius = Double
type Height = Double
type Width = Double
data Shape = Circle Radius
  | Square Height
  | Rectangle Height Width deriving Show

perimeter :: Shape -> Double
perimeter (Circle r) = 2*pi*r
perimeter (Square h) = 4*h
perimeter (Rectangle h w) = 2*h + 2*w

area :: Shape -> Double 
area (Circle r) = pi*r^2 
area (Square h) = h^2 
area (Rectangle h w) = h*w