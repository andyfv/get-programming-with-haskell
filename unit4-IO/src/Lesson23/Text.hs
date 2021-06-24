{-# LANGUAGE OverloadedStrings #-}

module Lesson23.Text where

import qualified Data.Text as T
import Data.Semigroup

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- 
sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- >>> T.lines sampleInput
-- ["this","is","input"]
--


someText :: T.Text
someText = "Some\ntext for\n you"

-- >>> T.words someText
-- ["Some","text","for","you"]
--



breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"


-- >>> T.splitOn breakText exampleText
-- ["This is "," to do"]
--


-- >>> T.unlines (T.lines sampleInput)
-- "this\nis\ninput\n"
--

-- >>> T.unwords (T.words someText)
-- "Some text for you"
--


-- >>> T.intercalate breakText (T.splitOn breakText exampleText)
-- "This is simple to do"
--



-- Use Semigroup to combine Text

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"



myLines :: T.Text -> [T.Text]
myLines text = T.splitOn "\n" text

myUnlines :: [T.Text] -> T.Text
myUnlines lst = T.intercalate "\n" lst
