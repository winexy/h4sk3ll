import qualified Data.Map as Map

users :: Map.Map Int String
users = Map.fromList [(1, "Ellie")]

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

mainMaybe :: Maybe String
mainMaybe = do
  name <- Map.lookup 1 users
  return (helloPerson name)