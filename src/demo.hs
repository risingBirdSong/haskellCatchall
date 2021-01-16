-- data Tree a = Node a [Tree a] deriving (Sgow)

-- instance Functor (Tree) where 
--   fmap f (Node value children) = Node (f value) (fmap (fmap f) children)

-- data Tree a = Node a [Tree a] deriving (Show, Eq)
-- instance Functor Tree where 
  -- fmap f (Node val sub) = Node (f val) (fmap (fmap f) sub) 

nums = [1,2,3]
justs = [Just 1,Just 2,Just 3, Nothing]
myTreeA = Node 100 [Node 12 [], Node 46 [Node 66 [Node 99 []]], Node 123 [Node 9 []]]
myTreeB = Node 101 [Node 13 [], Node 47 [Node 67 [Node 100 []]], Node 124 [Node 10 []]]

-- simpletree = Node "im the parent" [Node "im a child" [], Node "me too" [], Node "we all are" []]
-- counter ls = fmap (succ) ls 


data Tree a = Node a [Tree a] deriving (Show, Eq)

instance Functor (Tree) where 
  fmap f (Node val sub) = Node (f val) (fmap (fmap f) sub)



