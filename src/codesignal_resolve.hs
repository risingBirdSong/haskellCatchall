import Data.List
import Data.Char
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


libgroupByB _ [] = []
libgroupByB p' (x':xs') = (x':ys') : zs'
  where (ys',zs') = go p' x' xs'
        go p z (x:xs)
           | p z x = (x:ys, zs)
           | otherwise = ([], (x:ys) : zs)
             where (ys, zs) = go p x xs
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

-- https://stackoverflow.com/questions/44558242/picture-how-mapaccumr-works
coolPrint = let (_, result) =
                      mapAccumR 
                        (\cumulativeLength item -> 
                          let newLength = cumulativeLength + length item 
                          in (newLength, take (cumulativeLength) (repeat ' ') ++ item ++" ")
                        )
                        0
                        ["Geese", "Monkeys", "Chocolate", "Chips", "Dust", "Box"]
                    in mapM_ putStrLn $ reverse result
-- Box
--    Dust
--        Chips
--             Chocolate
--                      Monkeys
--                             Geese

isLucky n =  (sum frst == sum lst)
    where toList = map digitToInt $ show n
          frst = take ((length toList)`div`2) toList
          lst = drop ((length toList)`div`2) toList


digits' 0 = []
digits' n = digits' (n `div` 10) ++ [n `mod` 10]


-- https://discord.com/channels/280033776820813825/505367988166197268/816533687323328512

-- [1,2,3,4] should give
-- [1]
-- [2]
-- [3]
-- [4]
-- [1,2]
-- [2,3]
-- [3,4]
-- [1,2,3]
-- [2,3,4]

gauravsinghSubLists lst = reverse $ go 1 [] 
  where go cnt acc 
            | cnt == length lst = acc
            | otherwise = go (cnt + 1) ( divvy cnt 1 lst : acc)


cassesgreatidea =  [take 1, take 2, take 3] <*> tails [1,2,3,4]