type Pizza = (Double,Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size/2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = 
  if costP1 < costP2 then p1 else p2
  where costP1 = costPerInch p1
        costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++
                            " per square inch"
  where costSqInch = costPerInch (size,cost)


main :: IO () 
main = putStrLn "What is the size of pizza 1" >>
  getLine >>= 
  (\size1 -> 
    putStrLn "What is the cost of pizza 1" >>
    getLine >>= 
    (\cost1 -> 
      putStrLn "What is the size of pizza 2" >>
      getLine >>= 
      (\size2 -> 
        putStrLn "What is the cost of pizza 2" >>
        getLine >>= 
        (\cost2 -> 
          (\pizza1 -> 
            (\pizza2 ->
              (\betterPizza ->
                putStrLn (describePizza betterPizza)
              ) (comparePizzas pizza1 pizza2)
            ) (read size2, read cost2)
          ) (read size1, read cost1)
        )
      )
    )
  )


sizeData :: [Int]
sizeData = [12, 4, 9]

costData :: [Int]
costData = [100, 95, 24]

listMain :: [String]
listMain = do
  size1 <- [10, 12, 17]
  cost1 <- [12.0, 15.0, 20.0]
  size2 <- [10, 11, 18]
  cost2 <- [13.0, 14.0, 21.0]
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza pizza)
  
monadMain :: Monad a => m Double -> m Double -> m Double -> m Double -> m String
monadMain s1 c1 s2 c2 = do
  size1 <- s1
  cost1 <- c1
  size2 <- s2
  cost2 <- c2
  let betterPizza = comparePizzas (size1, cost1) (size2, cost2)
  return (describePizza betterPizza)