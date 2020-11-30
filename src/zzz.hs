-- solve([15,11,10,7,12]) = [15,7,12,10,11]
import Data.List

highAndLow ls = reverse $ highAndLow' (sort ls) []  
highAndLow' [] acc  = acc
highAndLow' [x] acc  = x : acc 
highAndLow' (x:ls) acc = highAndLow' (init ls) (x : last ls : acc)

hghLow_ ls = zip (sort ls) (reverse $ sort ls)
--  hghLow [15,11,10,7,12]
-- [(7,15),(10,12),(11,11),(12,10),(15,7)]
listy a b = [a,b]

hghLow ls = init $ concat $ take (length ls `div` 2 + 1)  $ reverse $  zipWith listy (sort ls) (reverse $ sort ls) 
hacky but I just wanted to see if it would pass all the tests, all tests passing :)
solve ls 
  | length (hghLow ls) == length ls =  hghLow ls 
  | otherwise = init $ hghLow ls 

h_l ls = reverse $ h_l' (sort ls) (reverse $ sort ls) [] 
h_l' [] _ acc = acc  
h_l' _ [] acc = acc  
h_l' (l:ls) (h:hs) acc  
  | l == h =  acc 
  | otherwise = h_l' ls hs (l:h:acc)


-- firstNonCons [] = Nothing 
firstNonCons [y] = Nothing 
firstNonCons (x:y:ls) 
  | (x + 1) /= y = Just y
  | otherwise = firstNonCons (y:ls)

sumOfParts ls = reverse $ sumOfParts' ls []
sumOfParts' [] acc = acc  
sumOfParts' (x:ls) acc = sumOfParts' ls (sum ls : acc )