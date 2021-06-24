{-# LANGUAGE OverloadedStrings #-}

module Lesson25.ByteString where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

-- sampleString :: String
-- sampleString = B.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack


nagarjunaBC :: BC.ByteString
nagarjunaBC = "नागर्जुनॅ"


nagarjunaText :: T.Text
nagarjunaText = "नागर्जुनॅ"

nagarjunaB :: B.ByteString
nagarjunaB = (BC.pack . T.unpack) nagarjunaText 


nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText

