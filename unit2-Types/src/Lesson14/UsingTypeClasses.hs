module UsingTypeClassed where

data NewEngland = ME | VT | NH | MA | RI | CT


-- Creatinf instance of a typeclass
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
                    deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
    show S1 = "1"
    show S2 = "2"
    show S3 = "3"
    show S4 = "4"
    show S5 = "5"
    show S6 = "6"


-- instance Eq SixSidedDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _ _   = False


-- too many !!! just use (deriving (Ord))
-- instance Ord SixSidedDie where
--     compare S6 S6 = EQ
--     compare S6 _  = GT
--     compare _ S6  = LT
--     compare S5 S5 = EQ
--     compare S5 _  = GT
--     compare _ S5  = LT
--     compare S4 S4 = EQ
--     compare S4 _  = GT
--     compare _ S4  = LT



-- instance Enum SixSidedDie where
--     toEnum 0 = S1
--     toEnum 1 = S2
--     toEnum 2 = S3
--     toEnum 3 = S4
--     toEnum 4 = S5
--     toEnum 5 = S6
--     toEnum _ = error "No such value"

--     fromEnum S1 = 0
--     fromEnum S2 = 1
--     fromEnum S3 = 2
--     fromEnum S4 = 3
--     fromEnum S5 = 4
--     fromEnum S6 = 5


newtype Name = Name (String, String) deriving (Show, Eq)

names :: [Name]
names = [ Name ("Emil", "Cioran")
        , Name ("Eugene", "Thacker")
        , Name ("Friedrich", "Nietzsche")
        ]

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = 
        compare (l1, f1) (l2, f2)



-- 1. 
data Number = One | Two | Three deriving Enum

instance Eq Number where
    (==) num1 num2 = (fromEnum num1) == (fromEnum num2)

instance Ord Number where
    compare num1 num2 = compare (fromEnum num1) (fromEnum num2)



-- 2. 
data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5
                    deriving (Show, Eq, Ord, Enum)


class (Eq a, Enum a) => Die a where
    roll :: Int -> a

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)
