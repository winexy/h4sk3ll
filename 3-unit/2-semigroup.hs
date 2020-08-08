import Data.Semigroup

instance Semigroup Integer where
  (<>) x y = x + y


data Color = Red |
  Yellow |
  Blue |
  Green |
  Purple |
  Orange |
  Brown |
  Transparent deriving (Show,Eq)


instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a Transparent = a
  (<>) Transparent a = a
  (<>) a b | a == b = a
          | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
          | all (`elem` [Blue,Yellow,Green]) [a,b] = Green 
          | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange 
          | otherwise = Brown
  

instance Monoid Color where 
  mempty = Transparent
  mappend = (<>)  


howMuch :: Int -> String
howMuch n | n > 10 = "a whole bunch"
          | n > 0 = "not much"
          | otherwise = "we're in debt!"