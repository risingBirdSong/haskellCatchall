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


-- test comment with Yoel

rs lst = rs' lst 0
rs' [] sum = []
rs' (x:xs) sum = rt : rs' (xs) rt
    where rt = (x+sum)



-- steps
-- find org max
-- dist candy gobl
-- convertto bool gte org

gc :: (Ord b, Num b) => [b] -> b -> [Bool]
gc lst e = map (>= maximum lst) (map (+e) lst) 



-- Great day of coding today, worked with a number of talented folks Vaibhav Kejriwal , Hao Fan, Yoel Morad, and Paul L Ferguson on code challenges.


-- It was great meeting up with all coding with all of them :)



-- But Yoel Morad and I really went into the deep end on a Leetcode challenge, called runningSum.



-- He wrote it easily in Javascript, I wrote mine in Haskell, each of us explaining our thought process as we went along.  The most awesome part for me was that he had never even seen Haskell code before, and solving this challenge prompted a ton of great questions from him and it truly was a crash course in Haskell. He was a fast learner and had a natural intuition for it, and documented everything quite thoroughly and became very interested in the language, and it's unique features. 😊



-- He even downloaded the compiler so that he could start writing his own Haskell code 😎.



-- Here are the  



--  https://leetcode.com/problems/running-sum-of-1d-array/submissions/

-- two step for pascal tri
-- build up the structure
-- do parent addition

buildStruct :: Num a => Int -> Int -> [[a]]
buildStruct str end
  | (str == end ) = []
  | (str < end) = (take str (repeat 1)) : buildStruct (str + 1) (end)

-- addParent prv idx strct 
  -- | () = 

tri = [[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]

mapSimple = [[1,1],[1,1,1]]
-- simple ar1 ar2 id = map () ar2'
    -- where ar1' = zipped ar1
          -- ar2' = zipped ar2

zipped arr = zip [0,1..] arr 

pascal 0 = [1]
pascal n = zipWith (+) ([0] ++ pascal (n-1)) (pascal (n-1) ++ [0])

pascals = [1] : map (\xs -> zipWith (+) ([0] ++ xs) (xs ++ [0])) pascals
-- like how?

fibszip = 0 : 1 : zipWith (+) fibszip (tail fibszip)

-- Rotaerk help break down this function with this nice graph
  -- 1, 1, 2, 3,  5,  8, 13
-- + 1, 2, 3, 5,  8, 13, 21
-----------------------
  -- 2, 3, 5, 8, 13, 21, 34

 

tester = [1] : map (\xs -> trace ("-->" ++ show xs  ) zipWith (+) ([0] ++ xs) (xs ++ [0])) tester


grida = [[4,3,2,-1],[3,2,1,-1],[1,1,-1,-2],[-1,-1,-2,-3]]

testa = [7,6,5,4,3,2,1]

filternegs lst = filter (\x -> x < 0) lst
-- countNegs mtrx

practice =  filter (>5) [1,2,3,4,5,6,7,8]


adjList = [3, 6, -2, -5, 7, 3]

-- adjacentElementsProduct :: [b] -> [(b, b)]
adjacentElementsProduct xs = maximum $ zipWith (*) xs (tail xs)


