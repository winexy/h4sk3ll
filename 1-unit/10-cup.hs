cup flOz = \message -> message flOz

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = 
  if ozDiff >= 0 
    then cup ozDiff
    else cup 0
  where 
    flOz = getOz aCup
    ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

coffeCup = cup 12
afterManySips = foldl drink coffeCup [1, 1, 1, 1, 1]