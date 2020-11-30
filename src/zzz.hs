-- solve([15,11,10,7,12]) = [15,7,12,10,11]
import Data.List

highAndLow ls = reverse $ highAndLow' (sort ls) []  
highAndLow' [] acc  = acc
highAndLow' [x] acc  = x : acc 
highAndLow' (x:ls) acc = highAndLow' (init ls) (x : last ls : acc)

-- firstNonCons [] = Nothing 
firstNonCons [y] = Nothing 
firstNonCons (x:y:ls) 
  | (x + 1) /= y = Just y
  | otherwise = firstNonCons (y:ls)

sumOfParts ls = reverse $ sumOfParts' ls []
sumOfParts' [] acc = acc  
sumOfParts' (x:ls) acc = sumOfParts' ls (sum ls : acc )