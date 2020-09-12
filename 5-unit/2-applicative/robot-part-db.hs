import qualified Data.Map as Map

data RobotPart = RobotPart {
  name :: String,
  description :: String,
  cost :: Double,
  count :: Int
} deriving Show
 
leftArm :: RobotPart
leftArm  = RobotPart
  { name = "left arm"
  , description = "left arm for face punching!"
  , cost = 1000.00
  , count = 3
  }

rightArm :: RobotPart
rightArm  = RobotPart
  { name = "right arm"
  , description = "right arm for kind hand gestures"
  , cost = 1025.00
  , count = 5
  }

robotHead :: RobotPart
robotHead  = RobotPart
  { name = "robot head"
  , description = "this head looks mad"
  , cost = 5092.25
  , count = 2
  }

leftLeg :: RobotPart
leftLeg  = RobotPart
  { name = "left robot leg"
  , description = "huuuuge leg"
  , cost = 100500
  , count = 5
  }

rightLeg :: RobotPart
rightLeg  = RobotPart
  { name = "right robot leg"
  , description = "huuuuge right leg"
  , cost = 42.5
  , count = 11
  }

robotDB :: Map.Map Int RobotPart
robotDB = Map.fromList (zip [1..5] [leftArm, rightArm, robotHead, leftLeg, rightLeg])

readRobotId :: String -> IO Int
readRobotId message = do
  putStrLn message
  id <- read <$> getLine
  return id

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "Missing item"
printCost (Just cost) = print cost

main :: IO ()
main = do
  firstRobotId <- readRobotId "Enter first robot id"
  secondRobotId <- readRobotId "Enter second robot id"
  let fstRobot = Map.lookup firstRobotId robotDB
  let sndRobot = Map.lookup secondRobotId robotDB
  let cheapest = min <$> (cost <$> fstRobot) <*> (cost <$> sndRobot)
  printCost cheapest