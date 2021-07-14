module Book where

import Lib
import Data.Aeson
import Data.Text                  as T
import Data.ByteString.Lazy       as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics


data Book = Book
    { title  :: T.Text
    , author :: T.Text
    , year   :: Int
    } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author = "Will Kurt"
              , title  = "Learn Haskell"
              , year   = 2017
              }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook


rawJSON :: BC.ByteString
rawJSON = "{\"year\":1949,\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\"}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Cioran\",\"title\":\"A Short History of Decay\",\"year\"=1949}"

bookFromWrongJSON :: Maybe Book 
bookFromWrongJSON = decode wrongJSON

-- QC 40.2
data Name = Name { firstName :: T.Text
                 , lastName  :: T.Text
                 } deriving (Show, Generic)

-- instance FromJSON Name
-- instance ToJSON Name

-- 
sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage { message   :: T.Text
                                 , erorrCode :: Int
                                 } deriving Show


instance FromJSON ErrorMessage where
    parseJSON (Object v) = ErrorMessage <$> v .: "message"
                                        <*> v .: "error"

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message errorCode) = 
        object [ "message" .= message
               , "error"   .= errorCode
               ]

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0


-- QC 40.3
instance FromJSON Name where
    parseJSON (Object v) = Name <$> v .: "firstName"
                                <*> v .: "lastName"

-- QC 40.4
instance ToJSON Name where
    toJSON (Name firstName lastName) = 
        object [ "firstName" .= firstName
               , "lastName"  .= lastName
               ]