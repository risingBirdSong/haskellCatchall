
data Tree v = Nil | Nodey (Tree v) v (Tree v) deriving (Show, Eq, Ord)

nsrt Nil v = Nodey Nil v Nil 
nsrt (Nodey l v r) n 
  | n == v = (Nodey l v r)
  | (n < v) = Nodey (nsrt l n) v r
  | (n > v) = Nodey l v (nsrt r n)  

testT = foldl nsrt (Nil) [5,4,6,3,7,2,8]
trTest = foldl nsrt (Nil) [4,2,7,1,3]

contains Nil qry = False
contains (Nodey l v r) qry
  | (v == qry) = True
  | (qry < v) = contains l qry 
  | otherwise = contains r qry

treeMap Nil _ = Nil
treeMap (Nodey l v r) fn = Nodey (treeMap l fn) (fn v) (treeMap r fn)

-- todo learn how to do tree fold, attempted at it went nowhere

rmv Nil _ = Nil
rmv (Nodey l v r) delV
  | (v == delV) = Nil
  | (delV < v) = Nodey (rmv l delV) v r
  | otherwise = Nodey l v (rmv r delV)

-- 700. Search in a Binary Search Tree

subT Nil _  = Nil
subT (Nodey l v r) qry 
  | qry == v = Nodey l v r
  | (qry < v) = subT l qry
  | (qry > v) = subT r qry 