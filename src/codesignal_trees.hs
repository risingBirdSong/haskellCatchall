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
import Data.Char
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

-- isSubtree t1 t2 = 
      -- where 

bigtree = Tree 5 (Tree 10 (Tree 4 (Tree 1 Null Null) (Tree 2 Null Null) ) (Tree 6 Null (Tree (-1) Null Null))) (Tree 7 Null Null)
smalltree = Tree 10 (Tree 4 (Tree 1 Null Null) (Tree 2 Null Null) ) (Tree 6 Null (Tree (-1) Null Null))


preorder Null = [] 
preorder (Tree v l r) =  [v] ++ (preorder l) ++ (preorder r)


postorder Null = []
postorder (Tree v l r) = (postorder l) ++ (postorder r) ++[v]

inorder Null = [] 
inorder (Tree v l r) =  (inorder l) ++ [v] ++ (inorder r)

isSubTree bg sm = (preorder sm) `isInfixOf` (preorder bg) && (postorder sm) `isInfixOf` (postorder bg)


restoreex = Tree 1 (Tree 2  (Tree 4 Null Null) Null) (Tree 3 (Tree 5 Null Null) (Tree 6 Null Null))
restoreexlist = Tree [1] (Tree [2]  (Tree [4] Null Null) Null) (Tree [3] (Tree [5] Null Null) (Tree [6] Null Null))


amap = M.empty 

largestValuesInTreeRows t = map maximum  $ M.elems $ buildTree t
buildTree t = go t 0 M.empty
  where go Null row themap = themap 
        go (Tree v l r) row themap 
            | M.member row themap = go r (row + 1) (go (l) (row + 1) (M.insertWith (++) row [v] themap))
            | otherwise = go r (row + 1) (go (l) (row + 1) (M.insert row [v] themap))
          


foldtest xs = foldr (+) 0 xs


-- fmap f bt



--      1
--    2   3
--  4    5  6


mergeMax :: Ord a => [a] -> [a] -> [a]
mergeMax (x:xs) (y:ys) = max x y : mergeMax xs ys
mergeMax xs [] = xs
mergeMax [] ys = ys

solution' :: Ord a => Tree a -> [a]
solution' Null = []
solution' (Tree v l r) = v : mergeMax (solution' l) (solution' r)


list1 = [1]


listRow Null = repeat []
listRow (Tree v l r) = [v] : zipWith (<>) (listRow l) (listRow r)
solver tree = map maximum . takeWhile (not . null) $ listRow tree 

listNum' (Tree v Null Null) = [[v]]
listNum' (Tree v Null r) = map (v :) (listNum' r)
listNum' (Tree v l Null) = map (v :) (listNum' l)
listNum' (Tree v l r) =  map (v:) $ (listNum' l) ++ (listNum' r)


digitTreeSum t = sum $ map (\x -> read x :: Int) $ map (map intToDigit) $ listNum' t

atree = Tree 1 (Tree 2  (Tree 4 Null Null) Null) (Tree 3 (Tree 5 Null Null) (Tree 6 Null Null))


traverseTree' Null  = repeat []
traverseTree' (Tree v l r)  = [v] : zipWith (<>) (traverseTree' l) (traverseTree' r)
traverseTree t = concat $ takeWhile (not . null) $ traverseTree' t