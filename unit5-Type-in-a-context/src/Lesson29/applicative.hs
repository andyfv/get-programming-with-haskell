

str1 :: Maybe String
str1 = Just "hello "

str2 :: Maybe String
str2 = Just "world"

combStrings :: Maybe String
combStrings = (++) <$> str1 <*> str2 


makeIOString :: IO String
makeIOString = pure "Hello World"

-- 
doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [500,20000]

-- totalPrize :: Int
-- totalPrize = (+) doorPrize boxPrize

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize


-- 
boxMultiplier = [10, 50]
newOutcomes = pure (*) <*> doorPrize <*> boxMultiplier

-- >>> newOutcomes
-- [10000,50000,20000,100000,30000,150000]
--

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where twoThroughN = [2 .. n]
          composite = pure (*) <*> twoThroughN <*> twoThroughN
          isNotComposite = not . (`elem` composite)



-- User
data User = User 
    { name :: String
    , gamerID :: Int
    , score :: Int
    } deriving Show


testNames :: [String]
testNames = 
    [ "John Smith"
    , "Robert'); DROP TABLE Students;--"
    , "Christina NULL"
    , "Randall Munroe"
    , "Andy"
    ]


testIds :: [Int]
testIds = 
    [ 1337
    , 0123
    , 999999
    ]

testScores :: [Int]
testScores = 
    [ 0 
    , 100000
    , -99999
    ]


testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores



-- 1. allMap
allMap :: Applicative f => (a -> b) -> f a -> f b
allMap f context = pure (f) <*> context


-- 2. 
example :: Maybe Int
example = pure (*) <*> pure ((+) 2 4) <*> pure 6


-- 3.
bought :: [Int]
bought = [6, 12]

lastNight :: [Int]
lastNight = 
    [ 2
    , 2
    ]

remainingBeer :: [Int]
remainingBeer = (\count -> count - 4) <$> lastNight


guests :: [Int]
guests = [2, 3]


totalPeople :: [Int]
totalPeople = (+ 2) <$> guests

beerPerGuest :: [Int]
beerPerGuest = [3,4]


totalBeersNeeded :: [Int]
totalBeersNeeded = (pure (*)) <*> beerPerGuest <*> totalPeople


beersToPurchase :: [Int]
beersToPurchase = pure (-) <*> totalBeersNeeded <*> remainingBeer
