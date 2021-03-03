import Data.List
import Data.Ord
import Data.List.Split
import qualified Data.List.GroupBy as Grp
import qualified Data.Map as M
import Debug.Trace

--                                                    myLib
-- *****************************************************************************************************

count :: (Ord a, Integral b) => [a] -> M.Map a b
count =
  foldr updateMap M.empty
    where updateMap v counts
            | M.member v counts = M.adjust succ v counts
            | otherwise           = M.insert v 1 counts

-- *****************************************************************************************************

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



commonCharacterCount s1 s2 = sum $ map snd $ M.toList $ M.intersectionWith min mA mB 
  where mA = count s1
        mB = count s2   


commonCharacterCount' sA sB = sum $ map (uncurry min) $ (\x -> (count x sA, count x sB)) <$> ['a'..'z']
  where count s xs = length $ filter (==s) xs  