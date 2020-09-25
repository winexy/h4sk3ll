head :: Monoid a => [a] -> a
head (x:_) = x
head [] = mempty

example :: [[Int]]
example = []

length :: Int
length = 8