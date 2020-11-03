-- 1299. Replace Elements with Greatest Element on Right Side
-- Input: arr = [17,18,5,4,6,1]
-- example   -- [18,17,6,5,4,1..]
-- Output:      [18,6,6,6,1,-1]


import Data.List

input = [17,18,5,4,6,1]
--srted=[18,17,6,5,4,1]
 
asc lst = sortBy (compare) lst
desc lst = sortBy (flip compare) lst 

countDrop cmp lst = countDrop' cmp lst 0 
countDrop' cmp [] cnt = cnt
countDrop' cmp (x:xs) cnt
  | (x >= cmp) = countDrop' cmp (xs) (cnt + 1)
  | otherwise = cnt

replace [x] = [-1]
replace (l:lst) = (maximum lst) : replace (lst)