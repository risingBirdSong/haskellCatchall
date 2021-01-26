import Data.List
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


collect Nil = []
-- collect (Nodey l v r) = (v : collect l)


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

btnsrt x Leaf = Node Leaf x Leaf
btnsrt x (Node l v r) 
  | x < v =  Node (btnsrt x l) v r
  | x > v = Node l v (btnsrt x r)
  | otherwise = Node l v r

-- testree :: BinaryTree Integer
atr = foldr btnsrt Leaf (reverse [5,4,6,3,7])
btr = foldr btnsrt Leaf (reverse [11,9,11,8,12])

testtree = foldr btnsrt Leaf $ reverse  [1,2,3,4,5]

fldTree fn acc Leaf = acc
fldTree fn acc (Node l v r) = fldTree fn (fldTree fn (fn v acc) l) r

buildBalanced []   = Leaf
buildBalanced elts = Node (buildBalanced $ take half elts) 
                          (elts !! half) 
                          (buildBalanced $ drop (half+1) elts)
    where half = length elts `quot` 2