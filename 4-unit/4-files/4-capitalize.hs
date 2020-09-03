import System.Environment
import System.IO
import qualified Data.Char as Char


capitalize :: String -> String
capitalize (head:tail) = Char.toUpper head : map Char.toLower tail
capitalize [] = []

capitalizeLine :: String -> String
capitalizeLine line = unwords capitalized
  where capitalized = map capitalize (words line)

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  let transformed = map capitalizeLine (lines input)
  let newFileName = "Capitalized__" ++ fileName
  writeFile newFileName (unlines  transformed)