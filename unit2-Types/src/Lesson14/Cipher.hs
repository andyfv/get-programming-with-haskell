module Cipher where

--  4 Letter Alphabet
data FourLetterAlphabet = L1 | L2 | L3 | L4
                          deriving (Show, Eq, Enum, Bounded)

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]


rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphaBet = (alphabetSize `div` 2)    -- find the middle of alphabet
          offset       = fromEnum c + halfAlphaBet
          rotation     = offset `mod` alphabetSize



rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where halfN  = n `div` 2
          offset = if even n
                   then fromEnum c + halfN
                   else fromEnum c + halfN + 1
          rotation = offset `mod` n


-- 3 Letter Alphabet
data ThreeLetterAlphabet = Alpha | Beta | Kappa
                            deriving (Show, Enum, Bounded)                            

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]


-- Putting All Together
rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar   = rotN alphaSize


rotDecoder :: String -> String 
rotDecoder text = map rotCharDecoder text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNdecoder alphaSize

-- 3 Letters
threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3l     = rotN alphaSize

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3decoder vals
    where alphaSize   = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3decoder = rotNdecoder alphaSize


-- 4 Letters
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4l     = rotN alphaSize


fourLetterDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterDecoder vals = map rot4ldecoder vals
    where alphaSize    = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4ldecoder = rotNdecoder alphaSize



-- XOR
xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)


type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : intToBits' nextVal
               else True  : intToBits' nextVal
               where remainder = n `mod` 2
                     nextVal   = n `div` 2


maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits  = reverse (intToBits' n)
          missingBits   = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle [False])


charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)


-- Convert back
bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
    where size          = length bits
          indices       = [size - 1, size - 2 .. 0]
          trueLocations = filter (\x -> fst x == True)
                                 (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)



-- One-time pad

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

-- Convert a String to Bits using One-time pad
applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = 
    map (\pair -> (fst pair) `xor` (snd pair))
        (zip padBits plainTextBits)
    where padBits       = map charToBits pad
          plainTextBits = map charToBits plainText



-- Encode Strings using One-time pad
applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
    where bitList = applyOTP' pad plainText


encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad



-- Cipher class
class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String


data Rot = Rot 

instance Cipher Rot where
    encode Rot text = rotEncoder text 
    decode Rot text = rotDecoder text


data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text


myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])



-- Pseudo-Random Number Generator (PRNG)
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber


examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100


-- 
data StreamCipher = StreamCipher Int

instance Cipher StreamCipher where
    encode (StreamCipher seed) = applySC seed
    decode (StreamCipher seed) = applySC seed


applySC :: Int -> String -> String
applySC seed plainText = map bitsToChar bitList
    where bitList = applySC' seed plainText

applySC' :: Int -> String -> [Bits]
applySC' seed plainText = zipWith xor randPadBits plainTextBits
    where randPad       = randList seed (length plainText)
          randPadBits   = map intToBits randPad
          plainTextBits = map charToBits plainText


randList :: Int -> Int -> [Int]
randList _ 0    = []
randList seed n = randNum : randList randNum (n - 1)
    where randNum = examplePRNG seed
