import Data.List
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace
import System.IO.Unsafe
import qualified Data.Set as S
import Data.Foldable
import Data.Maybe
import Data.Function
import Data.Tuple
import Data.Ord

--
-- Binary trees are already defined with this interface:
data Tree a = Tree { value :: a
                   , left :: Tree a
                   , right :: Tree a
                   } | Null deriving Show



myTree = Tree 1 (Tree 2 (Tree 3 Null Null) (Tree 4 Null Null) ) (Tree 2 (Tree 5 Null Null ) (Tree 6 Null Null))

hasPathWithGivenSum (Tree v Null Null) s = (s - v) == 0
hasPathWithGivenSum Null s = False
hasPathWithGivenSum (Tree v Null r) s = (hasPathWithGivenSum r (s-v))  
hasPathWithGivenSum (Tree v  l Null) s = (hasPathWithGivenSum l (s-v))  
hasPathWithGivenSum (Tree v l r) s = (hasPathWithGivenSum l (s-v)) || (hasPathWithGivenSum r (s-v))  
-- hasPathWithGivenSum t s = 

myOtherTreeA = Tree 1 (Tree 2 Null (Tree 3 Null Null)) (Tree 2 Null (Tree 5 Null Null))
myOtherTreeB = Tree 1 (Tree 2 Null (Tree 3 Null Null)) (Tree 2 Null (Tree 3 Null Null))

isTreeSymmetric Null = True
isTreeSymmetric (Tree v l r) = (sum ([v] ++ go l) == ( sum  ([v] ++ go r)))
go (Tree v Null Null) = [v] ++ [] 
go Null = [] 
go (Tree v Null r) = [v]  ++ (go r)
go (Tree v l Null) = [(-v)] ++ (go l)
go (Tree v l r) = [v] ++ (go l) ++ (go r)
-- isTreeSymmetric (Tree v l Null) = 1 + (isTreeSymmetric v l)