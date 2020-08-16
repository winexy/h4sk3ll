import qualified Data.Map as Map

quotes :: Map.Map String String
quotes = Map.fromList [
  ("1", "asd")
  , ("2", "sdf")
  , ("3", "dfg")
  , ("4", "ghj")
  , ("5", "hjk")]

getQuote :: [String] -> [String]
getQuote [] = []
getQuote ("n":xs) = []
getQuote (x:xs) = quote:(getQuote xs)
  where quote = case (Map.lookup x quotes) of 
                  Just text -> text
                  Nothing -> "Quote not found :("

main :: IO ()
main = do
  input <- getContents
  mapM_ print (getQuote (lines input))