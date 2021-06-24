

echo :: IO ()
echo = getLine >>= putStrLn

main :: IO ()
main = echo


echoV2 :: IO ()
echoV2 = 
    putStrLn "Enter a String an we'll echo it!" 
    >>  getLine 
    >>= putStrLn

-- 
readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

combine :: IO ()
combine = readInt >>= printDouble