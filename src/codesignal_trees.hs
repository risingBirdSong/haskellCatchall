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
isTreeSymmetric (Tree v l r) = ((sum $ map abs ([-v] ++ go l)) == ( sum  ([v] ++ go r)))
go (Tree v Null Null) = [v] ++ [] 
go Null = [] 
go (Tree v Null r) = [v]  ++ (go r)
go (Tree v l Null) = [(-v)] ++ (go l)
go (Tree v l r) = [-v] ++ (go l) ++ [v] ++ (go r)
-- isTreeSymmetric (Tree v l Null) = 1 + (isTreeSymmetric v l)

ktree = Tree 3 (Tree 1 Null Null) (Tree 5 (Tree 4 Null Null) (Tree 6 Null Null))

kthSmallestInBST t k = (appendTree t) !! (k-1) 

appendTree Null = []
appendTree (Tree v l r) =  (appendTree  l) ++ [v] ++ (appendTree r) 


kthSmallestInBST' t k = case kSmall t k of
    Left  l -> undefined -- went past the end
    Right r -> r

kSmall  Null        _ = Left 0
kSmall (Tree v l r) k = case kSmall l k of
    Right res -> Right res 
    Left  n   -> if k == n+1 
        then Right v
        else case kSmall r (k-(n+1)) of
            Left m -> Left (n+m+1)
            result -> result