data  Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show, Eq, Ord)

empty :: Tree a -> Bool
empty Nil = True
empty _ = False

insert :: Ord a => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node l v r) x
  | v == x = (Node l v r)
  | v < x = (Node (insert l v) x r)
  | v > x = (Node l x (insert r v))

  
contains Nil _ = False
contains (Node l v r) x
 | v == x = True
 | x < v = contains l x
 | x > v = contains r x

testTree = foldl insert Nil  [10,5,15,3,7,13,17]

delete Nil _ = Nil
delete (Node t1 v t2) x  
 | x == v = deleteX (Node t1 v t2)
 | x  < v = Node (delete t2 x) v t2
 | x  > v = Node t1 v (delete t2 x)

-- Delete root (is used on subtree)
deleteX :: (Ord a) => Tree a -> Tree a 
deleteX (Node Nil v t2) = t2
deleteX (Node t1 v Nil) = t1
deleteX (Node t1 v t2) = (Node t1 v2 t2) --(delete t2 v2))
 where 
  v2 = leftistElement t2
    
leftistElement :: (Ord a) => Tree a -> a
leftistElement (Node Nil v _) = v
leftistElement (Node t1 _ _) = leftistElement t1

deleteTwo = delete testTree 15

-- and what native data structures are implemented as trees?
-- Seq is a tree
-- Map and Set are trees
-- yea, they're all implemented as finger trees
-- https://en.wikipedia.org/wiki/Finger_tree
-- but also an advice that you generally dont go looking for trees

