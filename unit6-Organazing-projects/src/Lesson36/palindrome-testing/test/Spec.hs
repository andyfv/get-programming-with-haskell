import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char (isPunctuation)
import Data.Text as T


assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

prop_reverseInvariant text = 
    isPalindrome text == (isPalindrome (T.reverse text))

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1000} prop_reverseInvariant
    putStrLn "done!"



