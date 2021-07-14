module Lib
    ( isPalindrome
    , preprocess
    ) where

import Data.Char (isPunctuation)
import Data.Text as T


isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text


preprocess :: T.Text -> T.Text
preprocess text = T.filter (not . isPunctuation) text