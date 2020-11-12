data Tree v = No | Nodey (Tree v) v (Tree v) deriving (Show, Eq, Ord)

nsrt No v = Nodey No v No 
nsrt (Nodey l v r) n 
  | n == v = (Nodey l v r)
  | (n < v) = Nodey (nsrt l n) v r
  | (n > v) = Nodey l v (nsrt r n)  

testT = foldl nsrt (No) [5,4,6,3,7,2,8]

contains No qry = False
contains (Nodey l v r) qry
  | (v == qry) = True
  | (qry < v) = contains l qry 
  | otherwise = contains r qry

treeMap No _ = No
treeMap (Nodey l v r) fn = Nodey (treeMap l fn) (fn v) (treeMap r fn)

-- todo learn how to do tree fold, attempted at it went nowhere

rmv No _ = No
rmv (Nodey l v r) delV
  | (v == delV) = No
  | (delV < v) = Nodey (rmv l delV) v r
  | otherwise = Nodey l v (rmv r delV)