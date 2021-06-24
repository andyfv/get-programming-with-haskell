module Closures where

import Lesson4.FunctionsAsArguments



ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square


genIfEven f = (\x -> ifEven f x)

ifEvenInc' = genIfEven inc
-- ifEvenInc' = (\x -> ifEven inc x)

genIfXEven x = (\f -> ifEven f x)



getRequestURL host apiKey resource id = 
    host 
    ++ "/"
    ++ resource
    ++ "/"
    ++ id
    ++ "?token="
    ++ apiKey


genHostRequestBilder host = 
    (\apiKey resource id -> getRequestURL host apiKey resource id)


exampleBuilder = genHostRequestBilder "http://example.com"
