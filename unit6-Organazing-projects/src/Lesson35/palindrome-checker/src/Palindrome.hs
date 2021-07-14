module Palindrome
    ( isPalindrome
    ) where

import qualified Data.Text as T
import Data.Char (toLower, isSpace, isPunctuation)


stripWhiteSpaces :: T.Text -> T.Text
stripWhiteSpaces text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

preProcess :: T.Text -> T.Text
preProcess = stripWhiteSpaces . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preProcess text