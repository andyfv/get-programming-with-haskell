module Main where

import Lib
import Book
---------------------------------------
import Data.Aeson
import Data.Text                  as T
import Data.ByteString.Lazy       as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

------------------- DATA -------------------
-- NOAAResult
data NOAAResult = NOAAResult 
                    { uid          :: T.Text
                    , mindate      :: T.Text
                    , maxdate      :: T.Text
                    , name         :: T.Text
                    , datacoverage :: Double
                    , resultId     :: T.Text
                    } deriving Show

instance FromJSON NOAAResult where
    parseJSON (Object v) =
        NOAAResult <$> v .: "uid"
                   <*> v .: "mindate"
                   <*> v .: "maxdate"
                   <*> v .: "name"
                   <*> v .: "datacoverage"
                   <*> v .: "id"

-- Resultset
data Resultset = Resultset
                    { offset :: Int
                    , count  :: Int
                    , limit  :: Int
                    } deriving (Show, Generic)

instance FromJSON Resultset

-- Metadata
data Metadata = Metadata 
                { resultset :: Resultset
                } deriving (Show, Generic)

instance FromJSON Metadata

-- NOAAResponse 
data NOAAResponse = NOAAResponse    
                        { metadata :: Metadata
                        , results  :: [NOAAResult]
                        } deriving (Show, Generic)

instance FromJSON NOAAResponse

--------------------- ACTIONS --------------------- 
printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
    forM_ results (print . name)
    
main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults  = results <$> noaaResponse
    printResults noaaResults


-- 1.
instance ToJSON NOAAResponse
instance ToJSON Metadata
instance ToJSON Resultset
instance ToJSON NOAAResult where
    toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
        object [ "uid"          .= uid
               , "mindate"      .= mindate
               , "maxdate"      .= maxdate
               , "name"         .= name
               , "datacoverage" .= datacoverage
               , "id"           .= resultId
               ]

-- 2.
data IntList = EmptyList 
             | Cons Int IntList
             deriving (Show, Generic)

instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList