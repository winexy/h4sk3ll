import qualified Data.Map as Map

quotes :: Map.Map String String
quotes = Map.fromList [
  ("1", "asd")
  , ("2", "sdf")
  , ("3", "dfg")
  , ("4", "ghj")
  , ("5", "hjk")]

withDefaultQuote :: Maybe String -> String
withDefaultQuote (Just quote) = quote
withDefaultQuote Nothing = "Quote not found :("

getQuote :: [String] -> [String]
getQuote [] = []
getQuote ("n":xs) = []
getQuote (x:xs) = (withDefaultQuote quote):(getQuote xs)
  where quote = Map.lookup x quotes

main :: IO ()
main = do
  input <- getContents
  mapM_ print (getQuote (lines input))