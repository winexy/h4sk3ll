assignToGroups n xs = zip groups xs 
  where groups = cycle [1..n]

repeat n = cycle [n]

subseq start n xs = take n (drop start xs)

inFirstHalf x xs = elem x (take (div (length xs) 2) xs)

