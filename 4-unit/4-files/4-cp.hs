import System.Environment
import System.IO
import Data.List

main :: IO ()
main = do
  [from, to] <- getArgs
  content <- readFile from
  writeFile to content