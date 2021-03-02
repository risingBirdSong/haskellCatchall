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


almostIncreasingSequence s = and $ ((<2) . length . filter (uncurry (>=)) . zip s) . tail <$> [s, tail s]

