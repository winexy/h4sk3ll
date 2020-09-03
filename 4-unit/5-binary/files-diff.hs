import System.Envronment
import qualified Data.Text.IO as TI
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- B.readFile fileName
  putStrLn "Bytes:"
  print (B.length input)
  putStrLn "Characters:"
  print ((T.length . E.decodeUtf8) input)