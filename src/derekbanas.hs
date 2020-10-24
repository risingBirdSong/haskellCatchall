import System.IO
import Data.List
boundedInt = maxBound :: Int -- 9223372036854775807

-- Integer is unbounded whole number can be as big as memory can hold, huge number

-- prefer Doubles over floats, up to 11 points decimal precision


-- prefix vs infix
moda = mod 5 4
modb = 5 `mod` 4


negNumsNeedParens = 5 + (-4)

-- sqrt :: Floating a => a -> a
myNumA = 16 :: Int
-- in order to work with sqrt which takes Floating, we must convert
aaa = sqrt (fromIntegral myNumA )-- 4.0

myNums = [3,5,7,9,11]
-- Check if value is in list
is7InList = 7 `elem` myNums -- True

-- Get max value
maxPrime = maximum myNums -- 11
 
-- Get minimum value
minPrime = minimum myNums -- 3

-- You can use letters as well
letterList = ['A','C'..'Z'] -- "ACEGIKMOQSUWY"

infA = [11,22..]
access50 = infA !! 50 -- 561

-- We can filter the results with conditions
listTimes3 = [x * 3 | x <- [1..20], x*3 <= 50]
 
-- Return all values that are divisible by 13 and 9
divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

-- cool alt to standard factorial
productFact n = product [1..n]

-- example of as pattern
-- We can also get values with an As pattern
getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is "
  ++ [x]
  

  -- nice example
-- Check if strings are equal with recursion
areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

-- chainging recursive &&s in the above func areStringsEq
chainedAnds = True && True && True && True --True

-- ---------- EXAMPLE : FIBONACCI SEQUENCE ----------
 
-- Calculate the Fibonacci Sequence
-- 1, 1, 2, 3, 5, 8, ...
 
-- 1 : 1 : says to add 2 1s to the beginning of a list
-- | for every (a, b) add them
-- <- stores a 2 value tuple in a and b
-- tail : get all list items minus the first
-- zip creates pairs using the contents from 2 lists being the lists fib and the 
-- list (tail fib)
 
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib) ]
 
-- First time through fib = 1 and (tail fib) = 1
-- The list is now [1, 1, 2] because a: 1 + b: 1 = 2
 
-- The second time through fib = 1 and (tail fib) = 2
-- The list is now [1, 1, 2, 3] because a: 1 + b: 2 = 3
 
fib300 = fib !! 300 -- Gets the value stored in index 300 of the list
 
-- take 20 fib returns the first 20 Fibonacci numbers