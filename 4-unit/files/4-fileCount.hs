import System.Environment
import System.IO

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (charCount, wordCount, lineCount) =
  unwords [
    "chars: "
    , show charCount
    , " words: " 
    , show wordCount
    , " lines: "
    , show lineCount
  ]


main :: IO ()
main = do
  args <- getArgs
  let fileName = head args 
  file <- openFile fileName ReadMode
  input <- hGetContents fileName
  hClose file
  let summary = (countsText . getCounts) input
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary