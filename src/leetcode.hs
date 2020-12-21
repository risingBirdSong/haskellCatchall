import Debug.Trace
import Data.List
a = [1,1]
b = [2,2]
c = [1,2] 
d = [2,3]

e = [2]
f = [1,3]

g = [1,2,5]
h = [2,4]

fairCandy as bs = go
  where avg = (sum as + sum bs) `div` 2
        go = [(a,b) | a <- as, b <- bs, ((sum as) - a + b) == avg]
             


smaller as bs = head $ filter (\x -> (sum x) == (min (sum as) (sum bs))) [as,bs]

smaller_ as bs 
  | sum as < sum bs = as
  | otherwise = bs 

theOtherList theOne choice@[as,bs] = head $ filter ((/=) theOne ) choice 
