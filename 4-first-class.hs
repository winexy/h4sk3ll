import Data.List

ifEven :: (Int -> Int) -> Int -> Int
ifEven fn n = 
  if even n
  then fn n 
  else n

ifEvenCube = ifEven (\x -> x ^ 3) 
ifEvenInc = ifEven (\x -> x + 1)

names = [("Ian", "Curtis"),
        ("Bernard","Sumner"),
        ("Peter", "Hook"),
        ("Stephen","Morris")]

compareLastNames name1 name2 =
  if res == EQ
  then compare (fst name1) (fst name2) 
  else res
  where res = compare (snd name1) (snd name2)

addressLetter name location = locationFn name
  where locationFn = getLocationFunction location

addrssLetterV2 = flip addressLetter

addressLetterNY = addrssLetterV2 "ny"

sfOffice name = if lastName < "L"
  then nameText
      ++ " - PO Box 1234 - San Francisco, CA, 94111"
  else nameText
      ++ " - PO Box 1010 - San Francisco, CA, 94109"
   where lastName = snd name
         nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

dcOffice name = (fst name) ++ " " ++ (snd name) ++ " - Washington, DC"

getLocationFunction location = case location of 
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))