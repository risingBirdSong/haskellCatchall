import Data.List 
import Control.Monad
-- #elements not following ascending order
count_inversion :: [Int] -> Int
count_inversion [] = 0
count_inversion (e:es) = length (filter (e>) es) + count_inversion es

-- count_inversion_merge :: [Int] -> Int
count_inversion_merge = fst . count_inversion_merge'
  where
    count_inversion_merge' :: [Int] -> (Int, [Int])
    count_inversion_merge' [] = (0, [])
    count_inversion_merge' [x] = (0, [x])
    count_inversion_merge' l =
      let
        (left, right) = splitAt ((length l) `div` 2) l
        (left_n, left_sorted) = count_inversion_merge' left
        (right_n, right_sorted) = count_inversion_merge' right
        (merge_n, merge_sorted) = merge left_sorted right_sorted (0, [])
      in
        (left_n + right_n + merge_n, merge_sorted)

    merge :: [Int] -> [Int] -> (Int, [Int]) -> (Int, [Int])
    merge (x:xs) (y:ys) (acc, sorted)
      | x > y     = merge (x:xs) ys (acc + length (x:xs), sorted ++ [y])
      | otherwise = merge xs (y:ys) (acc                , sorted ++ [x])
    merge xs ys (acc, sorted) = (acc, sorted ++ xs ++ ys)


-- areSimilar :: [Int] -> [Int] -> Bool
areSimilar xs ys =  ((sort xs) == (sort ys)) && 
  ((<=1) . abs $ (count_inversion_merge xs) - count_inversion_merge ys ) 

a :: [Int]
a = [832, 998, 148, 570, 533, 561, 894, 147, 455, 279]
b :: [Int]
b = [832, 998, 148, 570, 533, 561, 455, 147, 894, 279]


aa :: [Int]
aa = [4, 6, 3]
bb :: [Int]
bb = [3, 4, 6]