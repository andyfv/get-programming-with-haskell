
import qualified Data.Map as Map
import qualified Data.List as List

data Grade  = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
    { candidateId :: Int
    , codeReview  :: Grade
    , cultureFit  :: Grade
    , education   :: Degree
    } deriving Show


viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where passedCoding     = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMin     = education candidate >= MS
          tests            = [ passedCoding
                             , passedCultureFit
                             , educationMin
                             ]


testCandidate :: Candidate
testCandidate = Candidate
    { candidateId = 1
    , codeReview  = A
    , cultureFit  = A
    , education   = PhD
    }

-- >>> viable testCandidate
-- True


---------------------- IO Context -----------------------

readInt :: IO Int
readInt = getLine >>= return . read

readGrade :: IO Grade
readGrade = do
    input <- getLine
    let grade = read input
    return grade
-- readGrade = getLine >>= return . read

readDegree :: IO Degree
readDegree = getLine >>= return . read

readCandidate :: IO Candidate
readCandidate = do
    putStrLn "enter id:"
    cId <- readInt
    putStrLn "enter code grade:"
    codeGrade <- readGrade
    putStrLn "enter culture fit grade:"
    cultureGrade <- readGrade
    putStrLn "enter education:"
    degree <- readDegree
    return (Candidate { candidateId = cId
                      , codeReview  = codeGrade
                      , cultureFit  = cultureGrade
                      , education   = degree
                      })

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed
                    then "passes"
                    else "failed"
    return statement



--------------- Maybe Context --------------------

candidate1 :: Candidate
candidate1 = Candidate
    { candidateId = 1
    , codeReview  = A
    , cultureFit  = A
    , education   = BA
    }

candidate2 :: Candidate
candidate2 = Candidate 
    { candidateId = 2
    , codeReview  = C
    , cultureFit  = A
    , education   = PhD
    }

candidate3 :: Candidate
candidate3 = Candidate
    { candidateId = 3
    , codeReview  = A
    , cultureFit  = B
    , education   = MS
    }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [ (1, candidate1)
                           , (2, candidate2)
                           , (3, candidate3)
                           ]


assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDB
    let passed = viable candidate
    let statement = if passed
                    then "passes"
                    else "failed"
    return statement


-- 
failPassOrElse :: Maybe String -> String
failPassOrElse Nothing    = "error: id not found"
failPassOrElse (Just val) = val 


--------------------- List Context ---------------------
candidates :: [Candidate]
candidates = 
    [ candidate1
    , candidate2
    , candidate3
    ]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passes"
                    else "failed"
    return statement


---------------------- Monadic Function ----------------------
assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement



-- 1. 
main :: IO ()
main = 
    putStrLn "What is the size of pizza 1"
    >> getLine 
    >>= (\size1 ->
        putStrLn "What is the cost of pizza 1"
        >> getLine 
        >>= (\cost1 ->
            putStrLn "What is the size of pizza 2"
            >> getLine 
            >>= (\size2 ->
                putStrLn "What is the cost of pizza 2"
                >> getLine 
                >>= (\cost2 ->
                    let pizza1      = (read size1, read cost1)
                        pizza2      = (read size2, read cost2)
                        betterPizza = comparePizzas pizza1 pizza2
                    in  putStrLn (describePizza betterPizza)
                    )
                )
            )
        )

type Pizza = (Double,Double)      

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size


comparePizzas :: Pizza -> Pizza -> Pizza 
comparePizzas p1 p2 = 
    if costP1 < costP2
    then p1
    else p2 
    where costP1 = costPerInch p1 
          costP2 = costPerInch p2 


describePizza :: Pizza -> String
describePizza (size,cost) = 
    "The " ++ show size ++ " pizza " ++
    "is cheaper at " ++ show costSqInch ++
    " per square inch"
    where costSqInch = costPerInch (size,cost)


-- 2.
listMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
listMain s1 c1 s2 c2 = do
    size1 <- s1
    cost1 <- c1
    size2 <- s2
    cost2 <- c2
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2 
    return (describePizza betterPizza)
