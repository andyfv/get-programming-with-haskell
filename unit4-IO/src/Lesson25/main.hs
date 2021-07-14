module Lesson25.Main where

import System.Environment
import Control.Monad
import Lesson25.Glitcher as Glitcher
import qualified Data.ByteString.Char8 as BC


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched  <- foldM (\bytes func -> func bytes) imageFile Glitcher.glitchActions
    let gltichtedFileName = mconcat ["glitched_",fileName]
    BC.writeFile gltichtedFileName glitched
    print "all done!"