import System.Environment
import System.Random
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Glitch (glitchActions)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- foldM (\bytes func -> func bytes) imageFile Glitch.glitchActions
  let glitchedFileName = mconcat ["glitched_",fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"