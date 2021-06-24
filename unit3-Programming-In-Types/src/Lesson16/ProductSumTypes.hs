module Lesson16.ProductSumTypes where

-- data AuthorName = AuthorName String String
-- data Book       = Author String String Int

data Book = Book 
    { author     :: Creator
    , isbn       :: String 
    , bookTitle  :: String
    , bookYear   :: Int
    , bookPrice  :: Double
    }

data VynilRecord = VynilRecord 
    { artist      :: Creator
    , recordTitle :: String
    , recordYear  :: Int
    , recordPrice :: Double
    }

data AuthorName = AuthorName
    { firstName :: String
    , lastName  :: String
    }


-- 
type FirstName  = String
type LastName   = String
type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLAst Char Char LastName
          | FirstNameWithTwoInits FirstName Char Char
          deriving (Show)

data Creator = AuthorCreator Author 
             | ArtistCreator Artist
             deriving (Show)


data Author = Author Name deriving (Show)
data Artist = Person Name | Band String deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator     
                (Author (TwoInitialsWithLAst 'H' 'P' "LoveCraft"))


data StoreItem = BookItem Book 
               | RecordItem VynilRecord
               | ToyItem CollectibleToy
               | PamfletItem Pamflet


data Pamflet = Pamflet
    { pamfletTitle       :: String
    , pamfletDescription :: String
    , pamfletcontact     :: String
    }

data CollectibleToy = CollectibleToy
    { name        :: String
    , description :: String
    , toyPrice    :: Double
    }


price :: StoreItem -> Double
price (BookItem book)       = bookPrice book
price (RecordItem record )  = recordPrice record
price (ToyItem toy)         = toyPrice toy
price (PamfletItem _)       = 0.0


madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"


madeBy' :: StoreItem -> String
madeBy' item = 
    case item of
        BookItem book     -> show (author book)
        RecordItem record -> show (artist record)
        _                 -> "unknown"



-- 2.
data Shape = CircleShape Circle
           | SquareShape Square
           | RectangleShape Rectangle

data Circle = Circle 
    { radius :: Double }

data Square = Square
    { side :: Double }

data Rectangle = Rectangle 
    { a :: Double
    , b :: Double
    }


computeShape :: Shape -> (Double, Double)
computeShape shape = (perimeter shape, area shape)


perimeter :: Shape -> Double
perimeter shape = 
    case shape of 
        CircleShape circle  -> 2 * pi * radius circle
        SquareShape square  -> 4 * side square
        RectangleShape rect -> (2 * a rect) + (2 * b rect) 

area :: Shape -> Double
area shape =
    case shape of
        CircleShape circle  -> pi * (radius circle ^ 2)
        SquareShape square  -> (side square) ^ 2
        RectangleShape rect -> (a rect) * (b rect) 