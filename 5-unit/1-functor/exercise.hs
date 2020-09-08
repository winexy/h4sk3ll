data Box a = Box a deriving Show

-- 1

instance Functor Box where
  fmap f (Box a) = Box (f a)

morePresents :: Int -> Box a -> Box [a]
morePresents n box = fmap (take n . repeat) box

-- 2

myBox :: Box Int
myBox = Box 1

box2 = Box myBox

unwrap :: Box a -> a
unwrap (Box v) = v

-- 3

