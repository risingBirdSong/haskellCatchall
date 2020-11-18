
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show, Eq, Ord)

goal = [5,3,7,1,6]

nsrt Nil new = Node Nil new Nil
nsrt (Node l v r) new 
  | (new < v) = Node (nsrt l new) v r
  | (new > v) = Node l v (nsrt r new)
  | (new == v) = Node l v r

test = foldl nsrt Nil goal
testb = foldr (flip nsrt) Nil goal

sumtree Nil = 0
sumtree (Node l x r) = (sumtree l) + x + (sumtree r)

inserter [] acc = acc
inserter (v:vs) acc = inserter vs (nsrt acc v)

-- todo list
-- contains? / find
-- tree map
-- remove nodes
-- how to insert a list of vals without using fold
-- ** visualize the tree **