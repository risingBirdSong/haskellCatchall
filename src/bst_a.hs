data Tree a = Node (Tree a) a (Tree a) | Nil deriving (Show, Eq, Ord)

nsrt Nil v = Node Nil v Nil   
nsrt (Node l a r) v
  | (v == a) = Node l v r
  | (v < a) = Node (nsrt l v) a r
  | (v > a) = Node l a (nsrt r v)

-- contains Nil v = False
-- contains (Node l a r) v
--   | v 

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
fld_a = foldl (nsrt) Nil [5,4,6,3,7,2,8]
--  (Node (Node (Node Nil 2 Nil) 3 Nil) 4 Nil) 5 (Node Nil 6 (Node Nil 7 (Node Nil 8 Nil)))

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
fld_b =  foldr (flip nsrt) Nil (reverse [5,4,6,3,7,2,8])
--  (Node (Node (Node Nil 2 Nil) 3 Nil) 4 Nil) 5 (Node Nil 6 (Node Nil 7 (Node Nil 8 Nil)))

contains Nil _ = False
contains (Node l v r) qry 
  | (v == qry) = True
  | (qry < v) = contains l qry
  | (qry > v) = contains r qry


smallest (Node l v r) 
  | l == Nil = v
  | otherwise = smallest l 

greatest (Node l v r)
  | r == Nil = v
  | otherwise = greatest r

fld_five = foldl (nsrt) Nil [1,2,3,4,5]
fld_15 = foldl (nsrt) Nil [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
count Nil = 0 
count (Node l v r) = 1 + (count l) + count (r) 
