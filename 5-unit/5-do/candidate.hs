import qualified Data.Map as Map

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree } deriving Show


viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding
                ,passedCultureFit
                ,educationMin]


testCandidate :: Candidate
testCandidate = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  , education = PhD }

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readGradeDo :: IO Grade
readGradeDo = do
  input <- getLine
  return (read input)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)


readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview = codeGrade
                    , cultureFit = cultureGrade
                    , education = degree })


assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement


candidate1 :: Candidate
candidate1 = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  , education = BA }

candidate2 :: Candidate
candidate2 = Candidate
  { candidateId = 2
  , codeReview = C
  , cultureFit = A
  , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate 
  { candidateId = 3
  , codeReview = A
  , cultureFit = B
  , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1)
                            ,(2,candidate2)
                            ,(3,candidate3)]


assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

failPassOrElse :: Maybe String -> String
failPassOrElse Nothing = "error id not found"
failPassOrElse (Just val) = val


candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

assessCandidate :: Monad m =>  m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement