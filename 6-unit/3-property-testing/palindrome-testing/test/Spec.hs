import Data.Text as T
import Data.Char
import Test.QuickCheck
import Test.QuickCheck.Instances
import Lib

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test
  then putStrLn passStatement
  else putStrLn failStatement

prop_punctuationInvariant :: T.Text -> Bool
prop_punctuationInvariant text = preProcess text == preProcess noPuncText 
  where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant :: T.Text -> Bool
prop_reverseInvariant text = isPalindrome text == (isPalindrome (T.reverse text))

prop_spaceInvariant :: T.Text -> Bool
prop_spaceInvariant text = preProcess text == preProcess noSpaceText
  where noSpaceText = T.filter (not . isSpace) text

-- prop_caseInvariant :: T.Text -> Bool
-- prop_caseInvariant text = preprocess text === toLowerCase

main :: IO ()
main = do
  putStrLn "Running tests..."
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  quickCheck prop_spaceInvariant
  putStrLn "done!"

  