import System.Random
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched  <- foldM (\bytes func -> func bytes) imageFile glitchActions
    let gltichtedFileName = mconcat ["glitched_",fileName]
    BC.writeFile gltichtedFileName glitched
    print "all done!"


glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = 
    [ randomReplaceByte
    , randomSortSection
    , randomReplaceByte
    , randomSortSection
    , randomReplaceByte
    , randomReverseBytes
    ]


intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = mod int 255


intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]


replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
    where (before,rest) = BC.splitAt loc bytes
          after = BC.drop 1 rest
          newChar = intToBC charVal


randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1, bytesLength)
    charVal  <- randomRIO (0, 255)
    return (replaceByte location charVal bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
    where (before, rest)  = BC.splitAt start bytes
          (target, after) = BC.splitAt size rest
          changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)



randomChar :: IO Char
randomChar = do
    randomInt <- randomRIO (0, 255)
    return (toEnum randomInt)


randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do 
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (reverseSection start sectionSize bytes)


reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, changed, after]
    where (before,rest)  = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          changed        = BC.reverse target