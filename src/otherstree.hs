data BinaryTree a = Empty 
    | Node (BinaryTree a) a (BinaryTree a) 
    deriving (Show)

-- insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a  
insert x Empty = Node (Empty) x (Empty)  
insert x (Node  left a right)   
    | x == a = Node left x right  
    | x < a  = Node (insert x left) a right  
    | x > a  = Node  left a (insert x right) 

-- bt = Node (Node (Leaf 4) 3 Empty) 1 (Leaf 2)
contains Empty query = False 
contains (Node l val r) query 
  | val == query = True 
  | query < val = contains l query
  | query > val = contains r query 

test = foldr insert (Empty) [15,5,10]
main = print (test)