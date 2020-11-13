data Tree a = Leaf a | Branch [Tree a] deriving (Show)

travTree                    :: Tree a -> [a]
travTree (Leaf x)           = [x]
travTree (Branch branches) = concat $ map travTree branches

testtree = Branch [Leaf 4, Leaf 5, Branch [Leaf 6, Leaf 7]]


toInsert = [Just 1, Nothing, Just 2 , Just 3, Nothing]
