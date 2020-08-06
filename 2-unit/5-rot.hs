data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Bounded, Enum)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where 
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]


fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder message = map (rotN alphabetSize) message
  where alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)


---


rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where 
    halfN = n `div` 2
    offset = if even n 
      then fromEnum c + halfN
      else 1 + fromEnum c + halfN 
    rotation = offset `mod` n


data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show,Enum,Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map (rotN alphaSize) vals
  where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)


threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map (rotNdecoder alphaSize) vals
  where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)


---


rotEncoder :: String -> String
rotEncoder text = map (rotN alphaSize) text
  where alphaSize = 1 + fromEnum (maxBound :: Char)

rotDecoder :: String -> String
rotDecoder text = map (rotNdecoder alphaSize) text
  where alphaSize = 1 + fromEnum (maxBound :: Char)


class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text