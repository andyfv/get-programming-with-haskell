{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Lesson34.Palindrome as Palindrome
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Enter a word and I'll know if it's a palindrome!"
    text <- TIO.getLine
    let response = if Palindrome.isPalindrome text
                   then "it is!"
                   else "it's not!"
    TIO.putStrLn response

