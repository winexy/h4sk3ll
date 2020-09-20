months :: [Int]
months = [31, 28, 31, 30, 29, 30, 31, 31, 30, 30, 30, 31]

calendar :: [Int] -> [Int]
calendar months = [date | end <- months, date <- [1..end]]

calendarDo :: [Int] -> [Int]
calendarDo months = do
  end <- months
  date <- [1..end]
  return date

calendarM :: [Int] -> [Int]
calendarM months = months >>= 
    (\end -> 
      [1..end] >>=
        (\date -> return date))