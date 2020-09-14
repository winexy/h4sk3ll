echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String an we'll echo it!" 
  >> getLine >>= putStrLn

main :: IO ()
main = echoVerbose


askForName :: IO ()
askForName = putStrLn "What is your name?"

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

main2 :: IO ()
main2 = askForName 
  >> getLine
  >>= return . greet
  >>= putStrLn