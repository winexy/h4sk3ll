maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM mpair = do
  (a, b) <- mpair 
  return (max a b)


echo :: IO ()
echo = getLine >>= putStrLn

doEcho :: IO ()
doEcho = do
  input <- getLine 
  putStrLn input