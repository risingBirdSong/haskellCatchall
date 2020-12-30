import Test.QuickCheck
import Data.List

myScan f init acc [] = acc 
myScan f init acc (l:ls) = myScan f (f init l) (acc ++ [(f init l)]) ls 

-- rngSum :: [Integer] -> [Integer]
rngSum ls = myScan (+) 0 [] ls 
invsRngSum ls = inverseScan (+) (rngSum ls) 

inverseScan f ls = go f [] (reverse ls) 
  where go f acc [] = acc
        go f acc [x] = x : acc
        go f acc (a:b:ls) =  go f (f a (negate b) : acc) (b:ls) 

runningSumCheck ls = ls == invsRngSum ls     
runningSumTest = verboseCheck (runningSumCheck :: [Int] -> Bool)
