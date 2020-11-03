-- 1299. Replace Elements with Greatest Element on Right Side
-- Input: arr = [17,18,5,4,6,1]
-- example   -- [18,17,6,5,4,1..]
-- Output:      [18,6,6,6,1,-1]


import Data.List

input = [17,18,5,4,6,1]

 
  
dropper _ [] = []
dropper cur (s:srtd) 
  | (s >= cur) = dropper cur (srtd)
  | (s < cur) = s : dropper cur (srtd)

replace [x] = [-1]
replace (l:lst) = (maximum lst) : replace (lst)