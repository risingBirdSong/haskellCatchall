-- reverse 

-- https://www.codewars.com/kata/523a86aa4230ebb5420001e1/train/haskell

-- factorial
import Data.List
import Debug.Trace

myLast_ :: [p] -> p
myLast_ [x] = x
myLast_ (x:xs) = myLast_ xs

--  [1 2 3]
--   [2 3]
--     [3]


-- numSplit :: Fractional a => a -> [a]
-- numSplit x `mod` 2 == 0 = [round x, round x]   
-- numSplit x 
--   | (x `mod` 2 == 0) = [round x, round]
--   | (x `mod` 2 /= 0) = [floor x, ceiling x]

-- invincible
-- numSplit :: (RealFrac a1, Integral a1, Integral a2) => a1 -> [a2]
-- numSplit :: (Num a1) => a1 -> [a2]
-- numSplit :: (RealFrac a1, Integral a1, Integral a2) => a1 -> [a2]
-- numSplit :: (RealFrac a, Integral b) => a -> [b]
-- numSplit :: (RealFrac a1, Integral a1, Integral a2) => a1 -> [a2]
-- numSplit :: (Integral a) => a -> [a]
numSplit :: Integral a => a -> [a]
numSplit x 
  | even x = [(x `div` 2), (x `div` 2)]
  | odd x = [x `div` 2, (x `div` 2) + 1]

rvrs :: [a] -> [a]
rvrs [] = [] -- end case , base case, exit condition 
rvrs (start:rest) = rvrs (rest) ++ [start] -- doing magic, doing the work

ttt = floor 5.5 

-- Input: nums = [1,2,3,4]
-- Output: [1,3,6,10]
-- Explanation: Running sum is obtained as follows: [1, 1+2, 1+2+3, 1+2+3+4].

runningSum lst = runningSum' lst 0
runningSum' :: Num a => [a] -> a -> [a]
runningSum' [] sum = []
runningSum' (x:xs) sum = (sum + x) : runningSum' (xs) (sum + x)


-- https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/

-- [2,3,5,1,3], extraCandies = 3
  
original = [2,3,5,1,3]


-- findMax :: [a] -> a
findMax lst = last (sort lst)

giveCandy lst extra = map (+extra) lst

greater lst max = map (\x -> if x >= max then True else False) lst

solve lst extra  = greater (distr extra) (max)
    where max = findMax lst
          distr = giveCandy lst


-- tester lst = map 

-- cur sum
--  1    1
--  2    3
--  3    6
--  4    10


rngsum lst = rngsum' lst 0  
rngsum' :: Num a => [a] -> a -> [a]
rngsum' [] sum = []
rngsum' (x:xs) sum = rng : rngsum' xs rng
    where rng = (x + sum)