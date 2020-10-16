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

testTree = foldl insert Nil  [1,2,3,4]