data Gtree a = Empty
             | Leaf a
             | Node a [Gtree a]
  deriving (Show)

a = Empty
b = Leaf 5
c = Node 4 [Leaf 5, Leaf 6, Leaf 7]
