xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2 ))

xorPair :: (Bool, Bool) -> Bool
xorPair (a, b) = xorBool a b

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)


---


type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
    then intToBits' nextVal ++ [False]
    else intToBits' nextVal ++ [True]
  where 
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length $ intToBits' maxBound

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ bits
  where bits = intToBits' n
        missingBits = maxBits - length bits
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
  where size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


---

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map xorPair (zip padBits plaintextBits)
  where 
    xorPair pair = fst pair `xor` snd pair
    padBits = map charToBits pad
    plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where bitList = applyOTP' pad plaintext


class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data OneTimePad = OTP String

instance Cipher OneTimePad where 
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])