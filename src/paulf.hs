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

solve lst = solve' lst (sortBy (flip compare) lst)
solve' _ [] = []
solve' [] _ = []
solve' (l:ls) (s:st) 
  | (s == l) = [s] : solve' (ls) (st)
  | (s >= l) = [s] : solve' (drop (countDrop l (s:st)) (l:ls)) (drop (countDrop l (s:st)) (s:st))
  | otherwise = [s] : solve' (ls) (s:st)

replace [x] = [-1]
replace (l:lst) = (maximum lst) : replace (lst)


replace' [_] = [-1]
replace' (_ : x : xs) = let (y:ys) = replace' (x:xs) in max x y : y : ys

replace'' [_] = [-1]
replace'' (_ : xs) = let ys = replace'' xs in max (head xs) (head ys) : ys

-- it's scanr1 max
-- just tail it and append -1
brilliantreplace = tail . scanr  max (-1) 
