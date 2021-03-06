{-# LANGUAGE OverloadedStrings #-}

module Lesson34.Palindrome 
    ( isPalindrome
    )
    where

import Data.Char (toLower, isSpace, isPunctuation)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

toLowerCase :: T.Text -> T.Text
toLowerCase text = T.toLower text

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text