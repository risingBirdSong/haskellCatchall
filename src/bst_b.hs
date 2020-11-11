data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show, Eq, Ord)

nsrt Nil a = Node Nil a Nil
nsrt (Node l val r) a 
  | val == a = Node l a r 
  | a < val = Node (nsrt l a) val r 
  | a > val = Node l val (nsrt r a)


inserted = foldl nsrt Nil [5,4,6,3,7]
-- Node (Node (Node Nil 3 Nil) 4 Nil) 5 (Node Nil 6 (Node Nil 7 Nil))
insertAgain = nsrt inserted 11
-- Node (Node (Node Nil 3 Nil) 4 Nil) 5 (Node Nil 6 (Node Nil 7 (Node Nil 11 Nil)))

contains Nil qry = False
contains (Node l v r) qry 
  | v == qry = True
  | qry < v = contains l qry
  | qry > v = contains r qry

treeMap Nil _ = Nil
treeMap (Node l v r) fn =  Node (treeMap l fn) (fn v) (treeMap r fn)

