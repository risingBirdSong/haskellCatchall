import Data.List
import Data.Ord
import Data.List.Split
import qualified Data.List.GroupBy as Grp

centuryFromYear y =  ((y-1) `div` 100) + 1

adjacentElementsProduct xs = maximum $ zipWith (*) xs (tail xs)

shapeArea n 
  | n == 1 = 1
  | otherwise = ((n-1) * 4) + shapeArea (n-1) 

makeArrayConsecutive2 n = length [(minimum n) .. (maximum n)] - length n


-- almostIncreasingSequence s = and $ ((<2) . length . filter (uncurry (>=)) . zip s) . tail <$> [s, tail s]


almostIncreasingSequence s = (filter (uncurry (>=)) . zip s) . tail <$> [s, tail s]

la = ["aba", 
 "aa", 
 "ad", 
 "vcd", 
 "aba"]

libgroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
libgroupBy _ [] = []
libgroupBy p' (x':xs') = (x' : ys') : zs'
  where
    (ys',zs') = go p' x' xs'
    go p z (x:xs)
      | p z x = (x : ys, zs)
      | otherwise = ([], (x : ys) : zs)
      where (ys,zs) = go p x xs
    go _ _ [] = ([], [])

libgroupByA _ [] = []
libgroupByA p' (x':xs') = (x' : ys') : zs'
  where 
    (ys',zs') = go p' x' xs' 
    go p z (x:xs)
       | p z x = (x : ys, zs)
       | otherwise = ([], (x:ys) : zs)
        where (ys,zs) = go p x xs 
    go _ _ [] = ([],[])

allLongestStrings xs = last $ libgroupBy (\x y -> length x == length y) $ sortBy (comparing length) xs

-- strange behavior of standard groupBy. why not split at 2?
-- *Main> groupBy (<) [1,2,3,2,3,4]
--[[1,2,3,2,3,4]]

-- more intuitive groupBy with libgroupBy
-- *Main> libgroupBy (<) [1,2,3,1,2,3]
-- [[1,2,3],[1,2,3]]

lb = [(1,2),(2,3),(3,1),(1,2),(2,3)]
