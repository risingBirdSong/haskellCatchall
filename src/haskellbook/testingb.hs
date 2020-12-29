import Test.QuickCheck
import Data.List
import Debug.Trace
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

mainA = quickCheck prop_revapp

mySplit del [] = []
mySplit del list =  takeWhile (/=del) list : mySplit del (drop 1 (dropWhile (/=del) (list)) )

